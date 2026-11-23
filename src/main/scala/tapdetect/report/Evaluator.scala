package tapdetect.report

import java.time.Duration
import tapdetect.engine.{Alarm, Pipeline}
import tapdetect.model.Span
import tapdetect.sim.{Simulator, TapEvent}

/** 离线评估：把告警和真值对齐，算检出率、误报率、定位误差。
  *
  * 这是整套系统"可验证"的部分。对安全类系统，没有评估就没有可信度 ——
  * 尤其是"极小损耗 + 强噪声"这种设定，只凭人工看几条曲线无法判断算法好坏。
  */
object Evaluator {

  final case class Truth(taps: Vector[TapEvent], spans: Map[String, Span], start: java.time.Instant)

  final case class Match(
      alarm: Alarm,
      tap: TapEvent,
      kmErrorM: Double,
      timeErrorHours: Double,
      withinWindow: Boolean,
      withinTime: Boolean
  )

  final case class Report(
      nTruth: Int,
      nAlarms: Int,
      matches: Vector[Match],
      falseAlarms: Vector[Alarm],
      missed: Vector[TapEvent],
      /** 检出率（按事件计；一个事件被多条告警命中只算一次） */
      recall: Double,
      /** 告警准确率（命中告警 / 全部告警） */
      precision: Double,
      /** 定位误差统计（仅命中的告警） */
      medianKmErrorM: Double,
      p90KmErrorM: Double,
      /** 在 ±windowM 内命中的比例 */
      withinWindowRate: Double,
      medianOnsetErrorHours: Double
  )

  /** 对齐规则：告警的缆段与真值一致、里程差 ≤ kmTol、起始时间差 ≤ timeTol。 */
  def evaluate(alarms: Vector[Alarm], truth: Truth,
               kmTolM: Double = 500.0, timeTolHours: Double = 240.0): Report = {
    val matches = scala.collection.mutable.ArrayBuffer.empty[Match]
    val matchedTaps = scala.collection.mutable.HashSet.empty[String]

    alarms.foreach { a =>
      val cands = truth.taps.filter { t =>
        t.spanId == a.spanId &&
          math.abs(t.km - a.km) * 1000.0 <= kmTolM &&
          math.abs(Duration.between(t.startAt, a.onset).toHours) <= timeTolHours
      }
      cands.sortBy(t => math.abs(t.km - a.km)).headOption match {
        case Some(t) =>
          val kmErr = math.abs(t.km - a.km) * 1000.0
          val tErr = math.abs(Duration.between(t.startAt, a.onset).toHours).toDouble
          matches += Match(a, t, kmErr, tErr,
            withinWindow = kmErr <= a.windowM,
            withinTime = tErr <= 48.0)
          matchedTaps += t.id
        case None =>
      }
    }

    val matchedAlarms = matches.map(_.alarm.id).toSet
    val falseAlarms = alarms.filterNot(a => matchedAlarms.contains(a.id))
    val missed = truth.taps.filterNot(t => matchedTaps.contains(t.id))

    val kmErrs = matches.map(_.kmErrorM).toArray
    val tErrs = matches.map(_.timeErrorHours).toArray

    Report(
      nTruth = truth.taps.length,
      nAlarms = alarms.length,
      matches = matches.toVector,
      falseAlarms = falseAlarms,
      missed = missed,
      recall = if (truth.taps.isEmpty) Double.NaN else matchedTaps.size.toDouble / truth.taps.size,
      precision = if (alarms.isEmpty) Double.NaN else matchedAlarms.size.toDouble / alarms.size,
      medianKmErrorM = if (kmErrs.isEmpty) Double.NaN else tapdetect.core.Stats.median(kmErrs),
      p90KmErrorM = if (kmErrs.isEmpty) Double.NaN else tapdetect.core.Stats.quantile(kmErrs, 0.9),
      withinWindowRate = if (matches.isEmpty) Double.NaN
        else matches.count(_.withinWindow).toDouble / matches.size,
      medianOnsetErrorHours = if (tErrs.isEmpty) Double.NaN else tapdetect.core.Stats.median(tErrs)
    )
  }

  def render(r: Report): String = {
    val sb = new StringBuilder
    def f2(x: Double) = if (x.isNaN) "n/a" else f"$x%.2f"
    sb.append("=== 评估结果 ===\n")
    sb.append(f"真值事件数：        ${r.nTruth}\n")
    sb.append(f"输出告警数：        ${r.nAlarms}\n")
    sb.append(f"命中告警数：        ${r.matches.length}\n")
    sb.append(f"检出率 (recall)：   ${f2(r.recall)}\n")
    sb.append(f"准确率 (precision)：${f2(r.precision)}\n")
    sb.append(f"定位误差中位数：    ${f2(r.medianKmErrorM)} m\n")
    sb.append(f"定位误差 P90：      ${f2(r.p90KmErrorM)} m\n")
    sb.append(f"落在搜索窗口内：    ${f2(r.withinWindowRate)}\n")
    sb.append(f"起始时间误差中位数：${f2(r.medianOnsetErrorHours)} 小时\n")
    if (r.missed.nonEmpty) {
      sb.append("\n--- 漏报 ---\n")
      r.missed.foreach { t =>
        sb.append(f"  ${t.spanId} @ ${t.km}%.3f km，损耗 ${t.excessLossDb}%.3f dB，起始 ${t.startAt}" +
          (if (t.episodes.nonEmpty) s"（间歇型，${t.episodes.length} 段）" else "（持续型）") + "\n")
      }
    }
    if (r.falseAlarms.nonEmpty) {
      sb.append("\n--- 误报 ---\n")
      r.falseAlarms.foreach { a =>
        sb.append(f"  ${a.id} ${a.spanId} @ ${a.km}%.3f km ±${a.windowM}%.0fm，" +
          f"损耗 ${a.lossDb}%.3f dB，置信 ${a.confidence}%.2f\n")
      }
    }
    // 按插损分档的召回率：说明"最小可检出插损"这条工程指标
    sb.append("\n--- 按插损分档 ---\n")
    val buckets = Vector(0.05, 0.10, 0.15, 0.25, 0.40, 1.0)
    val missedIds = r.missed.map(_.id).toSet
    buckets.sliding(2).foreach { case Vector(lo, hi) =>
      val inRange = r.matches.map(_.tap) ++ r.missed
      // 注意：matches 只含命中，missed 只含漏报；理论上应覆盖全部真值
      val hitsIn = r.matches.count(m => m.tap.excessLossDb >= lo && m.tap.excessLossDb < hi)
      val missesIn = r.missed.count(t => t.excessLossDb >= lo && t.excessLossDb < hi)
      val total = hitsIn + missesIn
      if (total > 0) {
        val pct = hitsIn.toDouble / total * 100.0
        sb.append(f"  $lo%.2f–$hi%.2f dB：命中 $hitsIn/$total（$pct%.0f%%）\n")
      }
    }

    sb.append("\n--- 命中明细 ---\n")
    r.matches.sortBy(-_.alarm.confidence).foreach { m =>
      sb.append(f"  ${m.alarm.id} ${m.tap.spanId}: 真值 ${m.tap.km}%.3f → 告警 ${m.alarm.km}%.3f km，" +
        f"误差 ${m.kmErrorM}%.0fm，窗口 ±${m.alarm.windowM}%.0fm，${if (m.withinWindow) "✓" else "✗"}；" +
        f"损耗真值 ${m.tap.excessLossDb}%.3f / 估计 ${m.alarm.lossDb}%.3f dB；" +
        f"置信 ${m.alarm.confidence}%.2f  ${m.alarm.severity}\n")
    }
    sb.toString
  }
}
