package tapdetect.engine

import tapdetect.core.{Inference, Stats}
import tapdetect.model._

/** 光功率监测分析。
  *
  * 光功率只有**整条链路一个标量**，不提供位置；但它是连续采样（分钟级），能看到
  * OTDR 看不到的"短暂窃听"和"起始时刻"。因此它的定位是：
  *   - OTDR 回答"在哪一段、大约哪里"；
  *   - 光功率回答"什么时候开始、持续多久、插损多大"。
  *
  * ## 模型
  *
  * 功率序列里有四类成分：
  *   1) 温度项 —— 熔接点损耗随盒温变化，幅度可达 0.1–0.3 dB，且是日周期；
  *   2) 老化/光源漂移 —— 缓慢单调，多天尺度；
  *   3) 施工震动 —— 短时随机抖动；
  *   4) 窃听 —— 某个时刻起**持续**的负台阶。
  *
  * 做法与 OTDR 一致：Y = a + b·T + c·t + ΔL·1[t≥τ]。温度项与时间项先回归掉，
  * 再在残差上找"线性 + 台阶"的最优拟合，台阶位置 τ 由拟合给出。
  *
  * 为什么不先用滑动中位数去趋势再 CUSUM：慢漂移的相关时间可达数周，与台阶在
  * 20–30 天窗口内共线；先"去趋势"会把台阶一起抹掉，再去 CUSUM 就什么也找不到
  * （本项目早期实现实测如此）。联合拟合让二者在同一模型里竞争，台阶不可被
  * 线性项解释，因此能保留。
  */
object PowerAnalyzer {

  final case class Config(
      /** 温度响应的滞后（分钟）：光纤与接头盒热惯性大 */
      thermalLagMinutes: Int = 90,
      /** 变点两侧最少样本数 */
      minSegment: Int = 24,
      /** 施工标志高于此值的样本剔除 */
      constructionReject: Double = 0.5,
      /** 最小可报告损耗 */
      minLossDb: Double = 0.03,
      /** 持续性门限 */
      minPersistence: Double = 0.6,
      /** 单条序列的族错误率（候选 τ 数的 Šidák 校正） */
      familyAlpha: Double = 0.01
  )

  final case class PowerVerdict(
      spanId: String,
      wavelengthNm: Int,
      lossDb: Double,
      zScore: Double,
      onsetTime: java.time.Instant,
      persistence: Double,
      /** 温度项对 20°C 温差的等效损耗（诊断用，越大越容易因温度误判） */
      tempAmplitudeDb: Double = 0.0,
      /** 时间项对 30 天的等效漂移（诊断用） */
      driftAmplitudeDb: Double = 0.0
  )

  def analyze(series: PowerSeries, env: EnvIndex, cfg: Config): Option[PowerVerdict] = {
    val n = series.length
    if (n < 4 * cfg.minSegment) return None
    val step = series.stepSeconds

    // 构造量：滞后温度、时间
    val lagSamples = math.max(0, cfg.thermalLagMinutes * 60 / step.toInt)
    val temp = new Array[Double](n)
    val timeDays = new Array[Double](n)
    var i = 0
    while (i < n) {
      temp(i) = env.boxTemp(series.timeAt(i).minusSeconds(lagSamples.toLong * step))
      timeDays(i) = i.toDouble * step / 86400.0
      i += 1
    }

    // 剔除施工期间的样本（下标映射：keep 里的位置 → 原序列位置）
    val keep = (0 until n).filter { k =>
      env.construction(series.timeAt(k)) <= cfg.constructionReject
    }.toArray
    if (keep.length < 4 * cfg.minSegment) return None
    val m = keep.length
    val y = new Array[Double](m)
    val tU = new Array[Double](m)
    val dU = new Array[Double](m)
    var k = 0
    while (k < m) {
      val src = keep(k)
      y(k) = series.valuesDbm(src)
      tU(k) = temp(src)
      dU(k) = timeDays(src)
      k += 1
    }

    // 第一步：回归掉温度与时间（这两项与台阶不共线，可以安全先扣）
    val m0 = Inference.huberRegression(Array(tU, dU), y)
    val resid = new Array[Double](m)
    k = 0
    while (k < m) { resid(k) = y(k) - m0.predict(Array(tU(k), dU(k))); k += 1 }

    // 第二步：在残差上找"某个时刻起持续变低"的区段。
    //
    // 不能只拟合"一个贯穿全序列的台阶"：间歇型窃听（接上、拆下、再接上）会让台阶
    // 在两段 episode 之间回到基线，单台阶模型会把两次 episode 平均掉，结果台阶高度
    // 减半甚至消失（本项目实测：0.27 dB 的间歇事件被拟合成 0.09 dB）。
    //
    // 正确做法：扫描所有 (start, end) 区间，找"区间内的中位数比区间外低得最多"的
    // 那一段。这正是 CUSUM 在两方向上的思想，但对"回到基线的脉冲"更稳健。
    val fit = findDipSegment(resid, cfg.minSegment)
    if (fit.isEmpty) return None
    val f = fit.get

    // 显著性：用台阶前后各自的稳健 σ，按中位数差的标准误
    // 区间内 = "低电平"段；区间外 = 基线
    val inside = java.util.Arrays.copyOfRange(resid, f.start, f.end)
    val outside = resid.take(f.start) ++ resid.drop(f.end)
    val sIn = Stats.robustSigma(inside)
    val sOut = Stats.robustSigma(outside)
    val sigma = {
      val a = if (sIn.isNaN || sIn <= 1e-12) sOut else sIn
      val b = if (sOut.isNaN || sOut <= 1e-12) sIn else sOut
      if (a.isNaN && b.isNaN) 1e-9 else math.sqrt((a * a + b * b) / 2.0)
    }
    val se = sigma * math.sqrt(math.Pi / 2.0 *
      (1.0 / math.max(1, inside.length) + 1.0 / math.max(1, outside.length)))
    val z = if (se <= 1e-15) 0.0 else f.stepDb / se

    // 多重比较：候选 (start, end) 组合数的保守上界
    val nTests = math.max(1, m / cfg.minSegment * m / cfg.minSegment)
    val alphaAdj = Inference.sidakAlpha(cfg.familyAlpha, nTests)
    val zGate = Inference.zCritical(alphaAdj)

    // 持续性：区间内样本相对基线的符号一致性
    val base = if (outside.isEmpty) 0.0 else Stats.median(outside)
    val persistence =
      if (inside.isEmpty) 0.0
      else inside.count(v => math.signum(v - base) == math.signum(f.stepDb)).toDouble / inside.length

    if (f.stepDb >= 0 || math.abs(z) < zGate ||
        math.abs(f.stepDb) < cfg.minLossDb || persistence < cfg.minPersistence) {
      return None
    }

    val onsetSrc = keep(f.start)
    Some(PowerVerdict(
      spanId = series.spanId,
      wavelengthNm = series.wavelengthNm,
      lossDb = -f.stepDb,
      zScore = math.abs(z),
      onsetTime = series.timeAt(onsetSrc),
      persistence = persistence,
      tempAmplitudeDb = math.abs(m0.slopes(0)) * 20.0,
      driftAmplitudeDb = math.abs(m0.slopes(1)) * 30.0
    ))
  }

  private final case class DipFit(start: Int, end: Int, stepDb: Double)

  /** 在残差上找"一段比基线低得最多"的区间（允许脉冲型，即区间后回到基线）。
    *
    * 用**前缀和 + 固定块长的滑动窗口**，O(n·(n/blk)) 但常数极小：
    * 对每个块起点，只尝试 1–3 个块长的区间（覆盖典型 episode 时长）。
    * 早期实现枚举所有 (start, end) 并对每段排序，复杂度 O(n³logn)，
    * 一条缆段要 5 秒，整网不可用。
    */
  private def findDipSegment(r: Array[Double], minSegment: Int): Option[DipFit] = {
    val n = r.length
    if (n < 3 * minSegment) return None
    val globalMed = Stats.median(r)

    val blk = minSegment
    // 块均值（用前缀和）
    val ps = new Array[Double](n + 1)
    var i = 0
    while (i < n) { ps(i + 1) = ps(i) + r(i); i += 1 }
    @inline def segMean(a: Int, b: Int): Double = if (b <= a) 0.0 else (ps(b) - ps(a)) / (b - a)

    var bestStart = -1
    var bestEnd = -1
    var bestDrop = 0.0

    val lengths = Vector(blk, 2 * blk, 3 * blk, 4 * blk)
    var s = 0
    while (s <= n - blk) {
      lengths.foreach { len =>
        val e = math.min(n, s + len)
        if (n - (e - s) >= minSegment) {
          val drop = globalMed - segMean(s, e)
          if (drop > bestDrop) { bestDrop = drop; bestStart = s; bestEnd = e }
        }
      }
      s += math.max(1, blk / 2)
    }
    if (bestStart < 0) None
    else {
      val inside = java.util.Arrays.copyOfRange(r, bestStart, bestEnd)
      Some(DipFit(bestStart, bestEnd, Stats.median(inside) - globalMed))
    }
  }
}
