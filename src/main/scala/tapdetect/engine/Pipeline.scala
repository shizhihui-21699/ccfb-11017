package tapdetect.engine

import tapdetect.model._

/** 端到端流程：原始观测 → 告警。
  *
  * 本类不关心数据来源（仿真 / 真实 OTDR 文件 / 流式采集），只要求上游提供：
  *   - 台账 Span（缆段、接头盒、熔接点）
  *   - 每个波长一组 OTDR 曲线
  *   - 每个波长一条光功率序列
  *   - 环境传感器读数（温度、施工强度）
  *
  * 处理顺序：
  *   1. 每个波长做时空变点扫描（ChangePointScanner）；
  *   2. 三波长按里程/时间配对，得到每处的"三波长插损谱"（OtdrAnalyzer）；
  *   3. 光功率独立分析，作为时间与幅度的旁证（PowerAnalyzer）；
  *   4. 多纤关联：同缆多根纤芯在同一里程同时异常（MultiFiberCorrelator）；
  *   5. 证据融合与全局多重比较校正（Fusion）。
  */
object Pipeline {

  final case class SpanData(
      span: Span,
      tracesByWavelength: Map[Int, Vector[OtdrTrace]],
      powerByWavelength: Map[Int, PowerSeries],
      env: Vector[EnvSample]
  )

  final case class Config(
      otdr: OtdrAnalyzer.Config = OtdrAnalyzer.Config(),
      power: PowerAnalyzer.Config = PowerAnalyzer.Config(),
      fusion: Fusion.Config = Fusion.Config(),
      /** 多纤关联：同一光缆内里程容差（m） */
      fiberKmToleranceM: Double = 300.0,
      /** 多纤关联：时间容差（小时） */
      fiberTimeToleranceHours: Double = 72.0
  )

  final case class Result(alarms: Vector[Alarm], diagnostics: Diagnostics)

  final case class Diagnostics(
      spansAnalyzed: Int,
      eventsFound: Int,
      candidates: Int,
      suppressedByZ: Int,
      suppressedByConfidence: Int,
      /** 单条缆段分析失败的数量（数据损坏/台账缺失等） */
      failedSpans: Int,
      failedSpanIds: Vector[String]
  )

  /** 处理大规模数据的入口。
    *
    * 真实环境的三个工程要求，在本方法里落实：
    *   1) **单条线路失败不能拖垮全局**：每条缆段的分析独立 try/catch，失败记入诊断后继续；
    *   2) **可并行**：缆段之间完全独立，按 CPU 数并行；
    *   3) **结果可复现**：并行不改变任何统计量（每条缆段的随机性只来自数据本身）。
    *
    * ⚠ 内存：本方法接收**全量**数据，因此调用方必须能一次性装下。一张上万条缆段的网
    * 的 OTDR 历史曲线可达 TB 级，绝不能这么做 —— 那种规模请用 `runStreaming`，
    * 它逐缆段拉取、分析完即释放。
    */
  def run(data: Vector[SpanData], cfg: Config): Result = {
    val spanByFiber = data.map(d => d.span.id -> d.span).toMap
    val failures = scala.collection.mutable.ArrayBuffer.empty[String]

    // 第一步：逐缆段独立分析（并行）。这是最耗时的部分。
    val perSpanEvents: Vector[(Span, Vector[OtdrAnalyzer.Event], Option[PowerAnalyzer.PowerVerdict])] =
      if (data.length <= 2) {
        data.map(d => analyzeOne(d, cfg, failures))
      } else {
        // 用 Java 的并行流而非 Scala 集合的 .par：Scala 3 的并行集合已拆到独立模块，
        // 而这里只需要"按 CPU 数并行映射"，标准库的 parallelStream 足够且无额外依赖。
        import scala.jdk.CollectionConverters._
        val results = new java.util.concurrent.ConcurrentLinkedQueue[(Span, Vector[OtdrAnalyzer.Event], Option[PowerAnalyzer.PowerVerdict])]()
        java.util.concurrent.ForkJoinPool.commonPool().submit(new Runnable {
          def run(): Unit = data.asJava.parallelStream().forEach { d =>
            results.add(analyzeOne(d, cfg, failures))
          }
        }).get()
        // 保持与输入相同的顺序，保证输出可复现
        val byId = results.asScala.map(r => r._1.id -> r).toMap
        data.map(d => byId.getOrElse(d.span.id, (d.span, Vector.empty, None)))
      }
    finish(perSpanEvents, spanByFiber, failures.toVector, data.length, cfg)
  }

  /** 流式入口：**逐缆段**拉取、分析、释放，内存占用只与单条缆段的长度和并行度有关。
    *
    * 这是真实部署应使用的入口。上游只要提供一个迭代器（从文件、对象存储或
    * Kafka 逐段读），本方法不会把它整体物化。
    *
    * 注意：传给 `produce` 的 `SpanData` 在返回后即不再被引用，可安全复用其缓冲区。
    */
  def runStreaming(data: Iterator[SpanData], totalSpans: Int, cfg: Config): Result = {
    val spanByFiber = scala.collection.mutable.Map.empty[String, Span]
    val failures = scala.collection.mutable.ArrayBuffer.empty[String]
    val perSpan = scala.collection.mutable.ArrayBuffer.empty[(Span, Vector[OtdrAnalyzer.Event], Option[PowerAnalyzer.PowerVerdict])]

    // 分批并行：每批 batchSize 条，批内并行、批间串行 —— 峰值内存 = batchSize × 单段数据。
    val batchSize = math.max(2, Runtime.getRuntime.availableProcessors())
    while (data.hasNext) {
      val batch = data.take(batchSize).toVector
      batch.foreach(d => spanByFiber(d.span.id) = d.span)
      import scala.jdk.CollectionConverters._
      val results = new java.util.concurrent.ConcurrentLinkedQueue[(Span, Vector[OtdrAnalyzer.Event], Option[PowerAnalyzer.PowerVerdict])]()
      java.util.concurrent.ForkJoinPool.commonPool().submit(new Runnable {
        def run(): Unit = batch.asJava.parallelStream().forEach { d =>
          results.add(analyzeOne(d, cfg, failures))
        }
      }).get()
      perSpan ++= results.asScala.toVector.sortBy(_._1.id)
    }
    finish(perSpan.toVector, spanByFiber.toMap, failures.toVector, totalSpans, cfg)
  }

  /** 多纤关联 + 光功率旁证 + 融合。所有路径都只在"事件"层面操作，不接触原始曲线。 */
  private def finish(
      perSpanEvents: Vector[(Span, Vector[OtdrAnalyzer.Event], Option[PowerAnalyzer.PowerVerdict])],
      spanByFiber: Map[String, Span],
      failures: Vector[String],
      spansAnalyzed: Int,
      cfg: Config
  ): Result = {
    val spanEvents = perSpanEvents.flatMap { case (sp, evs, _) => evs.map(e => (sp, e)) }

    // 多纤关联：把全部事件汇总后按物理光缆聚类
    val hits = spanEvents.map { case (sp, e) =>
      MultiFiberCorrelator.FiberHit(
        spanId = sp.id,
        km = e.antennaKm,
        windowM = 40.0,
        lossDb = e.primaryLossDb,
        onset = e.onsetTime,
        score = e.maxZ
      )
    }.toVector
    val clusters = MultiFiberCorrelator.cluster(hits, spanByFiber,
      kmToleranceM = cfg.fiberKmToleranceM,
      timeToleranceHours = cfg.fiberTimeToleranceHours)

    val powerCache = perSpanEvents.map { case (sp, _, pw) => sp.id -> pw }.toMap
    val eventCountBySpan = spanEvents.groupBy(_._1.id).view.mapValues(_.length).toMap

    val candidates = spanEvents.map { case (sp, e) =>
      val km = e.antennaKm
      val cl = clusters.find { cl =>
        cl.cableId == sp.cableId && math.abs(cl.km - km) * 1000.0 <=
          cfg.fiberKmToleranceM + cl.windowM
      }
      val pw = powerCache.getOrElse(sp.id, None).filter { p =>
        math.abs(java.time.Duration.between(p.onsetTime, e.onsetTime).toHours) <= 7 * 24
      }
      Fusion.Candidate(sp, e, pw, cl, math.max(1, eventCountBySpan.getOrElse(sp.id, 1)))
    }.toVector

    // 说明：光功率**不**作为独立报警路径。
    //
    // 设计取舍（经过实测验证）：光功率只有整链一个标量，0.05–0.25 dB 量级的持续下降
    // 无法与光源漂移、连接器接触变化、温度对熔接点的综合效应分开。实测中，把它当作
    // 独立路径会带来与真实事件同量级甚至更多的误报，而且无法给出位置 —— 对巡线组
    // 没有可执行价值。它的正确定位是 **OTDR 候选的时间与幅度旁证**（见下面的 power 字段）。
    //
    // 间歇型窃听（接上→拆下→再接上）在这套设定下确实会有漏报，这是信息论上的限制：
    // OTDR 每天只测一两次，若测量恰好在"拆下"期间，曲线本身就没有台阶可看。

    val m = candidates.length
    val alphaAdj = tapdetect.core.Inference.sidakAlpha(cfg.fusion.familyAlpha, math.max(1, m))
    val zGate = tapdetect.core.Inference.zCritical(alphaAdj)
    val passZ = candidates.count(_.event.maxZ >= zGate)
    val rawAlarms = Fusion.fuse(candidates, cfg.fusion)
    // 去重：同一根纤芯在相近里程（< 2 倍搜索窗）上若报了多条，只保留置信度最高者。
    // 这类重复来自"同一台阶在相邻 τ 上都被检出"，对现场没有额外信息。
    val alarms = dedupe(rawAlarms)

    Result(alarms, Diagnostics(
      spansAnalyzed = spansAnalyzed,
      eventsFound = spanEvents.length,
      candidates = m,
      suppressedByZ = m - passZ,
      suppressedByConfidence = passZ - alarms.length,
      failedSpans = failures.length,
      failedSpanIds = failures
    ))
  }

  /** 单条缆段的分析；任何异常都被隔离，记入 failures 后返回空事件。
    *
    * 返回的元组**不含**原始曲线 —— 调用方持有它时，曲线的内存可以立即释放。
    * 光功率在缆段内就地分析完，也只保留判定结果。
    */
  private def analyzeOne(d: SpanData, cfg: Config,
                         failures: scala.collection.mutable.ArrayBuffer[String])
      : (Span, Vector[OtdrAnalyzer.Event], Option[PowerAnalyzer.PowerVerdict]) = {
    try {
      val envIdx = EnvIndex(d.env)
      val evs = OtdrAnalyzer.analyze(d.span, d.tracesByWavelength, envIdx, cfg.otdr)
      val pw = d.powerByWavelength.toVector.sortBy { case (wl, _) =>
        if (wl == d.span.primaryWavelengthNm) 0 else 1
      }.headOption.flatMap { case (_, ps) => PowerAnalyzer.analyze(ps, envIdx, cfg.power) }
      (d.span, evs, pw)
    } catch {
      case e: Throwable =>
        failures += d.span.id
        System.err.println(s"[warn] 缆段 ${d.span.id} 分析失败，已跳过：" +
          s"${e.getClass.getSimpleName}: ${e.getMessage}")
        (d.span, Vector.empty, None)
    }
  }

  /** 对同一根纤芯、相近里程的多条告警去重（保留置信度最高者）。 */
  private def dedupe(alarms: Vector[Alarm]): Vector[Alarm] = {
    val sorted = alarms.sortBy(-_.confidence)
    val kept = scala.collection.mutable.ArrayBuffer.empty[Alarm]
    sorted.foreach { a =>
      val dup = kept.exists { k =>
        k.spanId == a.spanId && math.abs(k.km - a.km) * 1000.0 <=
          math.max(k.windowM, a.windowM) * 2.0
      }
      if (!dup) kept += a
    }
    // 按置信度重新编号，使 id 序号与重要性一致
    kept.toVector.zipWithIndex.map { case (a, i) =>
      a.copy(id = f"A-${a.cableId}-${a.fiberNo}-${i + 1}%02d")
    }
  }
}
