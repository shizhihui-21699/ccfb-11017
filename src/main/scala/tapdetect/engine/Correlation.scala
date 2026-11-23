package tapdetect.engine

import tapdetect.core.{Dsp, Stats}
import tapdetect.model._

/** 三波长差分分析。
  *
  * 物理依据（见 sim.Physics.bendLossDb）：宏弯损耗随波长陡增，1625nm 的弯曲损耗约为
  * 1310nm 的 5–20 倍；而熔接点温度漂移、连接器损耗、老化几乎与波长无关。
  *
  * 因此对同一位置、同一时间窗：
  *    - 若 ΔL(1625) ≫ ΔL(1310)  → 弯曲（窃听最典型的特征）
  *    - 若 ΔL(1625) ≈ ΔL(1310)  → 温漂/熔接劣化（不是窃听）
  *
  * 做法：对每段，用"段差"序列（见 OtdrAnalyzer）在三个波长上分别求变点，比较幅度。
  * 这里不再重复整条曲线分析，而是提供跨波长的判定函数。
  */
object WavelengthAnalyzer {

  final case class BendEvidence(
      /** 观测到的各波长台阶损耗（dB） */
      lossDb: Map[Int, Double],
      /** 用最小二乘拟合的"弯曲半径"（mm），越小越像人工弯折 */
      fittedRadiusMm: Double,
      /** 与"纯弯曲"模型的拟合优度（0..1） */
      bendModelR2: Double,
      /** 与"与波长无关的损耗"模型的吻合度（0..1）：越高越不像弯曲 */
      flatModelR2: Double,
      /** 最长波长 / 最短波长的损耗比：宏弯时通常 > 2 */
      longShortRatio: Double,
      verdict: String
  )

  /** 给定三波长的台阶幅度，判断是否符合宏弯特征。
    *
    * 只有 3 个点时，任何 3 点模型都能"拟合"得很好，R² 失去区分力 —— 实测 R² 在纯弯曲
    * 数据上也只有 ~0.95。所以主判据用**波段间的相对比值**（物理上由弯曲半径决定），
    * R² 只作为辅助。这样在只有三个波长的真实采集条件下才稳健。
    */
  def classify(lossDb: Map[Int, Double]): BendEvidence = {
    val ws = lossDb.keys.toVector.sorted
    if (ws.length < 2) {
      return BendEvidence(lossDb, Double.NaN, 0.0, 0.0, 1.0, "insufficient-wavelengths")
    }
    val ys = ws.map(w => lossDb(w)).toArray

    // 模型 A：纯弯曲，半径 r 为待估参数
    var bestR = Double.NaN
    var bestSse = Double.PositiveInfinity
    var r = 5.0
    while (r <= 20.0) {
      val pred = ws.map(w => PhysicsShape.bendRelative(r, w)).toVector
      val (_, sse) = scaleFit(pred, ys)
      if (sse < bestSse) { bestSse = sse; bestR = r }
      r += 0.25
    }
    val bendPred = ws.map(w => PhysicsShape.bendRelative(bestR, w)).toVector
    val (bendScale, _) = scaleFit(bendPred, ys)
    val bendScaled = bendPred.map(_ * bendScale)
    val bendR2 = rSquared(ys, bendScaled)

    // 模型 B：与波长无关（常数）
    val flat = Array.fill(ys.length)(Stats.mean(ys)).toVector
    val flatR2 = rSquared(ys, flat)

    // 主判据：最长波与最短波的损耗比。宏弯在真实弯曲半径下必然 > 2；
    // 温漂/熔接劣化则接近 1。
    val shortW = ys(0)
    val longW = ys.last
    val ratio =
      if (math.abs(shortW) < 1e-3) {
        if (math.abs(longW) < 1e-3) 1.0 else Double.PositiveInfinity
      } else longW / shortW

    val verdict =
      if (ratio > 1.8 && bendR2 > 0.85) "bend-like"
      else if (ratio < 1.35 && flatR2 > 0.9) "wavelength-flat"
      else "inconclusive"

    BendEvidence(lossDb, bestR, bendR2, flatR2, ratio, verdict)
  }

  /** 最小二乘整体比例因子：y ≈ scale·pred。scale = (pred·y)/(pred·pred)。 */
  private def scaleFit(pred: Vector[Double], ys: Array[Double]): (Double, Double) = {
    val n = math.min(pred.length, ys.length)
    var spp = 0.0; var spy = 0.0
    var i = 0
    while (i < n) { spp += pred(i) * pred(i); spy += pred(i) * ys(i); i += 1 }
    val scale = if (spp <= 1e-15) 0.0 else spy / spp
    var sse = 0.0
    i = 0
    while (i < n) { val d = ys(i) - scale * pred(i); sse += d * d; i += 1 }
    (scale, sse)
  }

  private def rSquared(ys: Array[Double], pred: Vector[Double]): Double = {
    val m = Stats.mean(ys)
    var sst = 0.0; var sse = 0.0
    var i = 0
    while (i < ys.length) {
      sst += (ys(i) - m) * (ys(i) - m)
      val d = ys(i) - pred(i); sse += d * d
      i += 1
    }
    if (sst <= 1e-15) 1.0 else 1.0 - sse / sst
  }
}

/** 弯曲损耗的归一化形状 —— 直接复用物理模型，保证"生成"和"识别"用的是同一个物理。
  *
  * 这一点很重要：如果检测器用一个与真实光纤不同的模型去找弯曲，误判率会不可控。
  * 复用同一个函数是仿真里唯一干净的做法；真实部署时应把这里的模型换成由厂家
  * 宏弯数据标定出的曲线表。
  */
object PhysicsShape {
  /** 以 1550nm 为 1.0 的相对弯曲损耗。 */
  def bendRelative(radiusMm: Double, wavelengthNm: Int): Double = {
    val ref = tapdetect.sim.Physics.bendLossDb(radiusMm, 1.0, 1550)
    val here = tapdetect.sim.Physics.bendLossDb(radiusMm, 1.0, wavelengthNm)
    if (ref <= 1e-12) 1.0 else here / ref
  }
}

/** 多纤关联：同一物理光缆上的多根纤芯在同一里程、同一时段出现异常。
  *
  * 这是把"单纤定向窃听"（只弯一根纤）和"接头盒被打开"（整束纤被扰动）区分开的关键，
  * 也是把假警报压下去的关键：一条纤芯的温漂可能碰巧超过门限，四根同时超过几乎不可能。
  */
object MultiFiberCorrelator {

  final case class FiberHit(spanId: String, km: Double, windowM: Double,
                            lossDb: Double, onset: java.time.Instant, score: Double)

  final case class CableCluster(
      cableId: String,
      km: Double,
      windowM: Double,
      hits: Vector[FiberHit],
      fiberCount: Int,
      /** 同缆纤芯命中率 */
      hitRatio: Double,
      simultaneous: Boolean,
      verdict: String
  )

  /** 把候选点按物理光缆聚类：里程相近（容差内）、起始时间相近的算一处。 */
  def cluster(hits: Vector[FiberHit], spanByFiber: Map[String, Span],
              kmToleranceM: Double = 300.0,
              timeToleranceHours: Double = 36.0): Vector[CableCluster] = {
    val byCable = hits.groupBy(h => spanByFiber(h.spanId).cableId)
    byCable.toVector.flatMap { case (cableId, hs) =>
      val sorted = hs.sortBy(h => (h.km, h.onset))
      val groups = scala.collection.mutable.ArrayBuffer.empty[scala.collection.mutable.ArrayBuffer[FiberHit]]
      sorted.foreach { h =>
        val g = groups.find { g =>
          math.abs(g.head.km - h.km) * 1000.0 <= kmToleranceM &&
            math.abs(java.time.Duration.between(g.head.onset, h.onset).toMinutes) <=
              (timeToleranceHours * 60).toLong
        }
        g match {
          case Some(g) => g += h
          case None => groups += scala.collection.mutable.ArrayBuffer(h)
        }
      }
      val fibersInCable = spanByFiber.values.count(_.cableId == cableId)
      groups.toVector.map { g =>
        val distinctFibers = g.map(_.spanId).distinct
        val kmMean = g.map(_.km).sum / g.length
        val winMax = g.map(_.windowM).max
        // 同时性：各纤起始时间跨度
        val tMin = g.map(_.onset).min
        val tMax = g.map(_.onset).max
        val simultaneous = java.time.Duration.between(tMin, tMax).toHours <= timeToleranceHours.toLong
        val ratio = distinctFibers.length.toDouble / math.max(1, fibersInCable)
        val verdict =
          if (distinctFibers.length >= 2 && ratio >= 0.4) "closure-intrusion"
          else if (distinctFibers.length >= 2) "multi-fiber"
          else "single-fiber"
        CableCluster(cableId, kmMean, winMax, distinctFibers.map { id =>
          g.filter(_.spanId == id).maxBy(_.score)
        }.toVector, fibersInCable, ratio, simultaneous, verdict)
      }
    }
  }
}
