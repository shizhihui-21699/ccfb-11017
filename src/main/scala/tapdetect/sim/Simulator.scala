package tapdetect.sim

import java.time.{Duration, Instant}
import java.util.Random
import tapdetect.core.Stats
import tapdetect.engine.EnvIndex
import tapdetect.model._

/** 窃听事件定义：在某个里程上弯折光纤/加分光器。
  *
  * @param km            事件位置（相对本段起点）
  * @param excessLossDb  插入损耗（分光器 + 弯曲 + 熔接），典型 0.05–0.5 dB；
  *                      这是检测要抓的东西，量级与正常温漂相当，是全系统最难的设定。
  * @param bendRadiusMm  弯曲半径（决定三波长差分比）
  * @param turns         缠绕圈数
  * @param startAt       开始时间（窃听者通常在夜间施工）
  * @param rampSeconds   从 0 加到满损耗的过渡时间（弯折是逐渐扣上的，不会瞬间到位）
  * @param episodes      是否存在间断（窃听者可能间歇性使用：接上、拆下、再接上）
  */
final case class TapEvent(
    id: String,
    spanId: String,
    km: Double,
    excessLossDb: Double,
    bendRadiusMm: Double,
    turns: Double,
    startAt: Instant,
    rampSeconds: Double = 120.0,
    episodes: Vector[(Instant, Instant)] = Vector.empty
) {
  /** t 时刻的插损贡献（dB）：考虑 ramp 和多段 episode 的"出现过又消失"。 */
  def lossAt(t: Instant, refWavelengthNm: Int = 1550): Double = {
    val active = episodes match {
      case e if e.isEmpty =>
        val dt = Duration.between(startAt, t).getSeconds.toDouble
        if (dt < 0) 0.0
        else if (rampSeconds <= 0) 1.0
        else math.min(1.0, math.pow(dt / rampSeconds, 0.6)) // 扣合过程在早期快、后期慢
      case eps =>
        eps.filter { case (a, b) => !t.isBefore(a) && !t.isAfter(b) }
          .map { case (a, _) =>
            val dt = Duration.between(a, t).getSeconds.toDouble
            if (rampSeconds <= 0) 1.0 else math.min(1.0, math.pow(dt / rampSeconds, 0.6))
          }.sum.min(1.0)
    }
    active * excessLossDb
  }

  /** 已存在的全部插损按波长分配。
    *
    * 物理分解：
    *   - **弯折/微弯耦合**部分：强波长相关（1625 ≫ 1550 ≫ 1310），是三波长判据的对象；
    *   - **分光器插片 + 熔接**部分：与波长近似无关。
    *
    * 比例由弯折半径决定：半径越小，弯折贡献越大（且越容易被发现）。
    * 对"隐蔽窃听"（R≈10–12mm），弯折贡献约占 60–80%，其余是分光器插片损耗。
    *
    * 早期实现把弯折系数写成 bend15/(bend15+0.10)，在 R=10.5mm 时弯折只占 1/3，
    * 于是三波长的差异被插片损耗稀释，检测器几乎无法区分"弯曲"和"温漂"。
    */
  def lossAtWavelength(t: Instant, wavelengthNm: Int): Double = {
    val total15 = lossAt(t, 1550)
    if (total15 <= 0) return 0.0
    val bend15 = Physics.bendLossDb(bendRadiusMm, turns, 1550)
    // 弯折占总插损的比例：由弯折半径决定。R 越小比例越高。
    // R=12mm → 0.5；R=10mm → 0.7；R=8mm → 0.9。
    val bendShare = math.max(0.4, math.min(0.95, 1.1 - 0.05 * (bendRadiusMm - 8.0) / 4.0 * 4.0))
    val bendPart = total15 * (if (bend15 > 1e-9) bendShare else 0.0)
    val flatPart = total15 - bendPart
    val ratio =
      if (bend15 <= 1e-9) 0.0
      else Physics.bendLossDb(bendRadiusMm, turns, wavelengthNm) / bend15
    flatPart + bendPart * ratio
  }

  def isActiveAt(t: Instant): Boolean = lossAt(t) > 1e-6
}

/** 仿真世界：多条光缆 × 多根纤芯 × 长时间序列的 OTDR 曲线和光功率。
  *
  * 关键设计：检测器只能看到 OtdrTrace / PowerSeries / EnvSample，
  * 以及 Span 台账（不含 TapEvent）。真值只用于评估。
  */
final case class World(
    spans: Vector[Span],
    taps: Vector[TapEvent],
    env: Vector[EnvSample],
    start: Instant
) {
  def envFor(spanId: String): Vector[EnvSample] = env.filter(_.spanId == spanId)
}

object Simulator {

  final case class Config(
      seed: Long = 20240117L,
      spanCount: Int = 8,
      fibersPerCable: Int = 4,
      lengthKmRange: (Double, Double) = (28.0, 62.0),
      spliceSpacingKm: (Double, Double) = (2.5, 4.5),
      start: Instant = Instant.parse("2024-01-01T00:00:00Z"),
      days: Int = 30,
      // OTDR 每天测 1–2 次（夜间维护窗口）
      otdrPerDay: Int = 1,
      powerStepSeconds: Long = 300L,
      tapRate: Double = 0.55, // 每段光缆被下手的概率
      // 环境噪声强度
      tempAmplitudeC: Double = 11.0,
      boxTempSwingC: Double = 16.0
  )

  private val sectionNames = Vector("直埋", "管道", "架空", "直埋", "管道")
  private val kinds = Vector("直埋接头盒", "架空接头盒", "人井")

  def buildWorld(cfg: Config): World = {
    val rng = new Random(cfg.seed)
    val spans = (0 until cfg.spanCount).flatMap { c =>
      (1 to cfg.fibersPerCable).map { f =>
        makeSpan(rng, cfg, cableIdx = c, fiberNo = f)
      }
    }.toVector

    // 窃听事件：并非每段都发生；且多发生在夜间、靠近接头盒的位置
    val taps = scala.collection.mutable.ArrayBuffer.empty[TapEvent]
    var tapIdx = 0
    spans.foreach { sp =>
      if (rng.nextDouble() < cfg.tapRate) {
        // 80% 的窃听发生在接头盒附近（真实统计规律：开盒最方便）
        val km =
          if (sp.closures.nonEmpty && rng.nextDouble() < 0.8) {
            val c = sp.closures(rng.nextInt(sp.closures.length))
            val off = (rng.nextDouble() - 0.5) * 0.30 // ±150m —— 真实的盒内/盒边位置
            math.max(0.3, math.min(sp.lengthKm - 0.3, c.km + off))
          } else {
            0.5 + rng.nextDouble() * (sp.lengthKm - 1.0)
          }
        val loss = 0.05 + rng.nextDouble() * 0.35 // 0.05–0.40 dB
        val radius = if (rng.nextDouble() < 0.4) 8.0 + rng.nextDouble() * 2.0
                     else 9.5 + rng.nextDouble() * 2.5
        val turns = 1.0 + rng.nextInt(3)
        val day = 1 + rng.nextInt(math.max(1, cfg.days - 8))
        val hour = 1 + rng.nextInt(4) // 凌晨 1–4 点
        val startAt = cfg.start.plus(Duration.ofDays(day.toLong)).plus(Duration.ofHours(hour.toLong))
        // 30% 的窃听是间歇性的（接上、拆掉、再接上）
        val eps =
          if (rng.nextDouble() < 0.3) {
            val a = startAt
            val b = startAt.plus(Duration.ofHours(30 + rng.nextInt(96)))
            val c = b.plus(Duration.ofDays(2 + rng.nextInt(5)))
            val d = c.plus(Duration.ofHours(48 + rng.nextInt(120)))
            Vector((a, b), (c, d))
          } else Vector.empty
        tapIdx += 1
        taps += TapEvent(s"TAP-$tapIdx", sp.id, km, loss, radius, turns, startAt,
          rampSeconds = 60 + rng.nextDouble() * 240, episodes = eps)
      }
    }

    // 环境：每段每 5 分钟一个样本（温度、施工）
    val env = spans.flatMap(sp => makeEnv(rng, cfg, sp)).toVector

    World(spans, taps.toVector, env, cfg.start)
  }

  private def makeSpan(rng: Random, cfg: Config, cableIdx: Int, fiberNo: Int): Span = {
    val (lo, hi) = cfg.lengthKmRange
    val len = lo + rng.nextDouble() * (hi - lo)
    val nSec = math.max(2, math.round(len / (4.0 + rng.nextDouble() * 3.0)).toInt)
    val secLen = len / nSec
    val sections = (0 until nSec).map { i =>
      val a = i * secLen
      val b = if (i == nSec - 1) len else (i + 1) * secLen
      CableSection(
        drumNo = cableIdx * 100 + i + 1,
        startKm = round3(a), endKm = round3(b),
        fromNode = s"J${cableIdx + 1}-${i + 1}",
        toNode = s"J${cableIdx + 1}-${i + 2}"
      )
    }.toVector

    val (slo, shi) = cfg.spliceSpacingKm
    val spliceKms = {
      val b = scala.collection.mutable.ArrayBuffer.empty[Double]
      var k = slo + rng.nextDouble() * 1.0
      while (k < len - 0.8) { b += round3(k); k += slo + rng.nextDouble() * (shi - slo) }
      b.toVector
    }

    val closures = spliceKms.zipWithIndex.map { case (km, i) =>
      Closure(s"${cableIdx + 1}#${i + 1}", s"J${cableIdx + 1}-${i + 2}", km,
        kinds(rng.nextInt(kinds.length)))
    }.toVector

    val splices = spliceKms.zipWithIndex.map { case (km, i) =>
      val mech = rng.nextDouble() < 0.12
      val poor = rng.nextDouble() < 0.10
      Splice(
        km = km,
        closureId = Some(closures(i).id),
        nominalLossDb = if (mech) 0.25 + rng.nextDouble() * 0.45
                        else (if (poor) 0.12 + rng.nextDouble() * 0.20
                              else 0.02 + rng.nextDouble() * 0.06),
        tempSensitivityDbPerC = if (poor) 0.004 + rng.nextDouble() * 0.016
                                else 0.0005 + rng.nextDouble() * 0.002,
        ageingDbPerYear = 0.002 + rng.nextDouble() * 0.02,
        mechanical = mech
      )
    }.toVector

    Span(
      id = s"C${cableIdx + 1}-F$fiberNo",
      label = f"环${cableIdx + 1}号 第$fiberNo 纤",
      cableId = s"C${cableIdx + 1}",
      fiberNo = fiberNo,
      lengthKm = round3(len),
      sections = sections,
      closures = closures,
      splices = splices,
      groupIndex = 1.468 + (rng.nextDouble() - 0.5) * 0.002
    )
  }

  /** 供测试场景复用：生成单段环境序列。noiseLevel 缩放温度扰动。 */
  def makeEnvPublic(rng: Random, cfg: Config, sp: Span, noiseLevel: Double): Vector[EnvSample] = {
    makeEnv(rng, cfg, sp).map(e => e.copy(
      boxTempC = 15.0 + (e.boxTempC - 15.0) * noiseLevel,
      airTempC = 15.0 + (e.airTempC - 15.0) * noiseLevel))
  }

  private def makeEnv(rng: Random, cfg: Config, sp: Span): Vector[EnvSample] = {
    val totalSeconds = cfg.days.toLong * 86400L
    val step = cfg.powerStepSeconds
    val n = (totalSeconds / step).toInt
    val phaseH = rng.nextDouble() * 24.0
    val airBase = 12.0 + rng.nextDouble() * 10.0
    val airAmp = cfg.tempAmplitudeC * (0.7 + rng.nextDouble() * 0.6)
    val boxOffset = 2.0 + rng.nextDouble() * 4.0
    // 慢变天气（多天尺度）：相关时间 4 天，稳态标准差 1.2°C
    val weatherTheta = Env.thetaFor(4.0 * 86400.0)
    val weather = Env.ouProcess(rng, n, 0.0, weatherTheta,
      Env.sigmaFor(1.2, weatherTheta), step.toDouble)
    // 施工事件：泊松到达，每次持续数小时
    val construction = new Array[Double](n)
    var i = 0
    while (i < n) {
      if (rng.nextDouble() < 3.0 / (86400.0 / step)) { // 平均每段每 8 小时不到一次
        val durS = 1800 + rng.nextInt(14400)
        val level = 0.3 + rng.nextDouble() * 0.7
        var j = i
        val end = math.min(n, i + (durS / step).toInt)
        while (j < end) { construction(j) = math.max(construction(j), level); j += 1 }
      }
      i += 1
    }

    (0 until n).map { k =>
      val t = cfg.start.plusSeconds(k.toLong * step)
      val air = Env.diurnal(t, airBase, airAmp, phaseH) + weather(k)
      // 盒内温度：比气温滞后约 2 小时、幅度更大（日照/密闭）
      val boxPhase = phaseH + 2.0
      val box = Env.diurnal(t, airBase + boxOffset, cfg.boxTempSwingC, boxPhase) +
        weather(k) * 0.6 + (rng.nextGaussian() * 0.25)
      EnvSample(sp.id, t, box, air, construction(k))
    }.toVector
  }

  private def round3(x: Double): Double = math.round(x * 1000.0) / 1000.0

  // ---------------------------------------------------------------------------
  // 观测生成
  // ---------------------------------------------------------------------------

  /** 生成一次 OTDR 曲线。
    *
    * 组成：
    *  - 背向散射随距离的指数衰减（用台账里的入纤衰减系数，含 ±5% 真实偏差）
    *  - 每个熔接点的损耗台阶 + 反射峰（机械接头）
    *  - 温度造成的损耗漂移
    *  - 施工震动造成的整条曲线瞬时抖动（不是台阶！这是能区分的关键）
    *  - 窃听弯折的局部插损
    *  - 采样噪声（随距离增加的信噪比劣化）
    *
    * 实现上按"断点累加"而不是"逐点遍历熔接点"：熔接点只有几十个，采样点有几十万个，
    * 逐点遍历是 O(n·m)，在一张网的规模上会成为瓶颈。
    */
  def otdrTrace(world: World, span: Span, at: Instant, wavelengthNm: Int,
                rng: Random, pulseWidthNs: Double = 20.0,
                sampleSpacingM: Double = 1.0,
                envIdx: EnvIndex = EnvIndex.empty): OtdrTrace = {
    val lengthKm = span.lengthKm
    val n = math.max(4, (lengthKm * 1000.0 / sampleSpacingM).toInt)
    val env = if (envIdx.isEmpty) EnvIndex(world.envFor(span.id)) else envIdx
    val (boxTemp, _, construction) = env.at(at)
    val refTemp = 15.0
    val years = Duration.between(world.start, at).toDays / 365.25
    val values = new Array[Double](n)

    val alphaPerM = span.attenuationDbPerKm.getOrElse(wavelengthNm, 0.20) / 1000.0
    // 真实入纤衰减与台账有 ±5% 偏差：检测器必须自己估，不能直接信台账
    val alphaTrue = alphaPerM * (0.95 + rng.nextDouble() * 0.10)
    // 曲线纵向整体偏移（每次测量发射功率、连接器损耗都不同）
    // 曲线纵向整体偏移：真实 OTDR 有发射功率/接收灵敏度自校准，曲线间偏移很小
    // （工程上 ~0.02 dB）。早期实现给了 ±0.25 dB，看起来"更真实"，实际是错的：
    // 相邻两次测量之间 0.25 dB 的独立抖动会把 0.1–0.3 dB 的窃听台阶整个淹没 ——
    // 实测导致 55 km 的缆段上 D 的噪声高达 0.2 dB，持续型 0.39 dB 的窃听漏检。
    val launchOffset = (rng.nextDouble() - 0.5) * 0.04
    // 施工震动：整条曲线的瞬时偏移 + 斜率变化，而非台阶
    val vib = construction
    val vibOffset = vib * rng.nextGaussian() * 0.12

    // 一次性算好每个熔接点在此温度/老化下的损耗，按位置排序做累积
    val spliceSteps: Array[(Int, Double)] = span.splices
      .map(sp => (math.round(sp.km * 1000.0 / sampleSpacingM).toInt,
        Physics.spliceLossAt(sp, boxTemp, refTemp, years)))
      .sortBy(_._1) // 按 index 排序
      .toArray
    val tapSteps: Array[(Int, Double)] = world.taps.filter(_.spanId == span.id).map { tap =>
      (math.round(tap.km * 1000.0 / sampleSpacingM).toInt, tap.lossAtWavelength(at, wavelengthNm))
    }.toArray

    var i = 0
    var cumLoss = 0.0
    var cursor = 0
    while (i < n) {
      while (cursor < spliceSteps.length && spliceSteps(cursor)._1 <= i) {
        cumLoss += spliceSteps(cursor)._2; cursor += 1
      }
      values(i) = -alphaTrue * (i * sampleSpacingM) + launchOffset + vibOffset - cumLoss
      i += 1
    }

    // 窃听事件：叠加在该位置之后的整段（台阶）
    tapSteps.foreach { case (c, add) =>
      if (add > 1e-6) {
        var j = math.max(0, c)
        while (j < n) { values(j) -= add; j += 1 }
      }
    }

    // 反射峰（机械接头）：窄脉冲下是几个采样点的尖峰
    span.splices.zipWithIndex.foreach { case (sp, idx) =>
      val h = Physics.reflectionHeightDb(sp.mechanical, poorQuality = idx % 7 == 0 && sp.mechanical)
      if (h > 0.5) {
        val c = math.round(sp.km * 1000.0 / sampleSpacingM).toInt
        val width = math.max(1, (pulseWidthNs / 10.0).round.toInt + 1)
        var d = -width
        while (d <= width) {
          val j = c + d
          if (j >= 0 && j < n) {
            val shape = math.exp(-0.5 * (d.toDouble / (width / 2.0 + 0.5)) * (d.toDouble / (width / 2.0 + 0.5)))
            values(j) += h * shape
          }
          d += 1
        }
      }
    }

    // 噪声：真实 OTDR 的单点噪声约 0.02–0.06 dB（随背向散射功率下降而缓慢增大），
    // 关键特征是**空间相关**：采样率远高于脉冲宽度决定的物理分辨率，相邻采样点共享
    // 同一个脉冲响应，所以噪声是低通的，不是白噪声。
    // 这里用一阶自回归（AR1）生成空间相关噪声；若按白噪声建模，检测器会低估
    // 段内中位数的不确定度，从而产生大量假台阶。
    val baseSigma = 0.045
    val sigmaAtEnd = 0.075
    val arCoef = 0.85
    var prev = 0.0
    var k = 0
    while (k < n) {
      val frac = k.toDouble / math.max(1, n - 1)
      val sigma = (baseSigma + (sigmaAtEnd - baseSigma) * frac) * (1.0 + 3.0 * vib)
      prev = arCoef * prev + math.sqrt(1.0 - arCoef * arCoef) * rng.nextGaussian()
      values(k) += prev * sigma
      k += 1
    }

    OtdrTrace(
      traceId = s"${span.id}-${at.toEpochMilli}-$wavelengthNm",
      spanId = span.id,
      wavelengthNm = wavelengthNm,
      measuredAt = at,
      startKm = 0.0,
      spacingM = sampleSpacingM,
      valuesDb = values
    )
  }

  /** 生成该段完整时间段的光功率序列（单波长）。 */
  def powerSeries(world: World, span: Span, wavelengthNm: Int, rng: Random,
                  cfg: Config, envIdx: EnvIndex = EnvIndex.empty): PowerSeries = {
    val step = cfg.powerStepSeconds
    val n = ((cfg.days.toLong * 86400L) / step).toInt
    val out = new Array[Double](n)
    val refTemp = 15.0
    // 光源/连接器基准功率（稳定，但有缓慢漂移）
    val base = -3.0 + rng.nextGaussian() * 0.05
    // 光源/接收机基线漂移：相关时间 ~2 周，稳态标准差 0.01 dB
    val driftTheta = Env.thetaFor(14.0 * 86400.0)
    val drift = Env.ouProcess(rng, n, 0.0, driftTheta, Env.sigmaFor(0.01, driftTheta), step.toDouble)
    val alphaPerKm = span.attenuationDbPerKm.getOrElse(wavelengthNm, 0.20)
    val fixedLoss = alphaPerKm * span.lengthKm +
      span.splices.map(_.nominalLossDb).sum
    val env = if (envIdx.isEmpty) EnvIndex(world.envFor(span.id)) else envIdx
    val spliceTempSensitivity = span.splices.map(_.tempSensitivityDbPerC).sum
    val spliceAgeing = span.splices.map(_.ageingDbPerYear).sum
    val tapsHere = world.taps.filter(_.spanId == span.id)
    var i = 0
    while (i < n) {
      val t = cfg.start.plusSeconds(i.toLong * step)
      val (boxTemp, _, construction) = env.at(t)
      val years = Duration.between(world.start, t).toDays / 365.25
      var p = base - fixedLoss + drift(i)
      // 温度对整条链路损耗的影响（接头温度系数 —— 这是 OPM 温度假警的主要来源）
      p -= spliceTempSensitivity * (boxTemp - refTemp)
      p -= spliceAgeing * years
      // 施工震动：瞬时波动
      p += construction * rng.nextGaussian() * 0.03
      // 窃听：分光器插损 + 取走的那一路光
      var s = 0
      while (s < tapsHere.length) {
        p -= tapsHere(s).lossAtWavelength(t, wavelengthNm)
        s += 1
      }
      // 接收机噪声
      p += rng.nextGaussian() * 0.002
      out(i) = p
      i += 1
    }
    PowerSeries(span.id, wavelengthNm, cfg.start, step, out)
  }
}
