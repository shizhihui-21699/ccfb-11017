package tapdetect.engine

import tapdetect.core.{Dsp, Inference, Stats}
import tapdetect.model._

/** OTDR 曲线的"平台差"检测器。
  *
  * ## 核心量
  *
  * 对一组同波长、同段的曲线，把它们按时间分成"前段"和"后段"（以候选变点 τ 为界），
  * 分别逐距离点取**中位曲线** P_pre[i]、P_post[i]，然后
  *
  *     D[i] = P_post[i] − P_pre[i]
  *
  * 物理含义：D[i] 是"在 i 之外的累积损耗变化"。在 i *之前* 的位置，两次的中位数
  * 曲线几乎相同（D≈0）；一旦越过一个新增的损耗台阶位置 k，D[i] ≈ −ΔL 并**保持**
  * 到下一个台阶。所以：
  *
  *   - 弯折窃听 → D 在 k 处出现**一次向下的阶跃**，之后是平台；
  *   - 熔接点温漂 → D 在**已知熔接点**处出现台阶（位置可解释）；
  *   - 施工震动 → D 两侧都有随机扰动，无明显平台；
  *   - 发射功率/斜率标定漂移 → D 是**整条曲线**的缓变项，不出现台阶。
  *
  * 关键优势：D 是**平台差**（不是斜率差），所以台阶高度直接可读，
  * 且不需要任何衰减斜率估计（P_pre 与 P_post 相减自动抵消）。
  *
  * ## 噪声与多重比较
  *
  * P_pre、P_post 是 M 条曲线的逐点中位数，噪声降到单次测量的 ~1.25/√M。
  * 沿空间方向再做高斯平滑（尺度 = 物理分辨率），平滑带来的相关性由
  * "独立分辨单元数"折算。最终在"独立单元数 × 候选 τ 数"上做 Šidák 校正。
  */
object ChangePointScanner {

  final case class Config(
      /** 变点两侧各自最少需要的测量次数 */
      minSegment: Int = 5,
      /** 施工等级高于此值的测量不参与 */
      constructionReject: Double = 0.5,
      /** 物理分辨率（m）：高斯平滑尺度取此值的一半 */
      resolutionM: Double = 20.0,
      /** 允许的最长"台阶平台"之外的漂移（m） */
      maxEventLengthM: Double = 500.0,
      /** 持续性门限：变点后与变点前符号一致的比例 */
      minPersistence: Double = 0.7,
      /** 一条缆段内的族错误率 */
      familyAlpha: Double = 0.01,
      /** 最小可报告损耗 */
      /** 最小可报告损耗。低于此值无法与噪声、温漂残余可靠区分 */
      minLossDb: Double = 0.05,
      /** 变点位置相对最近熔接点的最大距离（m）；更大时视为"非熔接点事件"，更可疑 */
      spliceProximityM: Double = 300.0
  )

  final case class SpatialEvent(
      edgeKm: Double,
      lossDb: Double,
      z: Double,
      persistence: Double,
      tau: Int,
      onsetTime: java.time.Instant,
      wavelengthNm: Int,
      /** 该台阶相对最近熔接点的距离（m）。很小的值提示"可能是熔接点温漂" */
      distToSpliceM: Double,
      /** 平台在边缘之后的稳定程度（0..1） */
      plateauQuality: Double
  )

  def scan(span: Span, traces: Vector[OtdrTrace], env: EnvIndex, cfg: Config): Vector[SpatialEvent] = {
    if (traces.length < 2 * cfg.minSegment) return Vector.empty
    val sorted = traces.sortBy(_.measuredAt)
    val usable = sorted.indices.filter { t =>
      env.construction(sorted(t).measuredAt) <= cfg.constructionReject &&
        !sorted(t).valuesDb.exists(_.isNaN)
    }.toArray
    val nT = usable.length
    if (nT < 2 * cfg.minSegment) return Vector.empty

    val (mat, kmAt, spacingM) = toMatrix(sorted, usable) // mat(t)(i)
    val nI = kmAt.length
    val smoothSigma = math.max(1.0, cfg.resolutionM / 2.0 / spacingM)
    val smoothRadius = math.max(2, math.round(3 * smoothSigma).toInt)

    // 每个候选 τ：前段/后段中位曲线之差，再平滑
    // 注意：不能用"所有 τ 都算一遍"（O(nT²·nI)）。用增量：从 τ 移到 τ+1 时，
    // 中位数更新是 O(nI log M)，可接受；这里直接按 τ 循环并对每列取中位数。
    val candidates = scala.collection.mutable.ArrayBuffer.empty[(Double, Double, Int, Double, Double, Int)]
    // (loss, |z|, tau, persistence, plateauQuality, stepIdx)

    // mat 的行 = 0..nT-1（对应 usable 的第 0..nT-1 个元素），因此 tau 直接就是行下标
    var tau = cfg.minSegment
    while (tau <= nT - cfg.minSegment) {
      val preIdx = (0 until tau).toArray
      val postIdx = (tau until nT).toArray
      val pre = medianProfile(mat, preIdx, nI)
      val post = medianProfile(mat, postIdx, nI)
      val d = Array.tabulate(nI)(i => post(i) - pre(i))

      // 把 D 拟合成"线性趋势 + 一个台阶"的模型：
      //
      //     D[i] = a + b·i + ΔL·1[i ≥ e] + 噪声
      //
      // 用**最小残差平方**在 (e, ΔL, a, b) 上搜索。这是唯一能同时处理
      // "衰减斜率差造成的线性项"和"局部台阶"的办法：
      //   - 单独去趋势（Theil–Sen/OLS）会把台阶当成斜率吸收掉 —— 实测 0.5→0.22；
      //   - 不建模线性项，则斜率差（30km 上可达 0.1dB）会伪造台阶 —— 实测大量假警报。
      // 联合拟合时，台阶在 e 处引入的不连续无法被线性项解释，因此两者可分离。
      //
      // 搜索是 O(nI²)，但可在分块粒度上做（块长 = 分辨率单元），代价可接受。
      val fit = fitLinearPlusStep(d, spacingM, cfg)
      // 台阶两侧都必须有足够的物理跨度，否则"平台"只是噪声起伏。
      val minSideM = math.max(cfg.resolutionM * 4, 150.0)
      val enoughSpan = fit.exists(f =>
        f.stepIdx * spacingM >= minSideM && (nI - f.stepIdx) * spacingM >= minSideM)
      // 重新在该位置用**平滑后**的 D 估计台阶高度。
      //
      // 为什么必须重估：拟合器是在原始 D 上按 SSE 选位置的，而 OTDR 的局部噪声可达
      // ±0.2dB（熔接点温漂 + 背向散射噪声），SSE 地形会在噪声坑里出现假极小值。
      // 实测：一条 0.39dB 的真实台阶被拟合成 0.05dB 的噪声台阶。
      // 平滑（尺度 = 物理分辨率）把噪声压掉一个数量级，而台阶是宽带阶跃不受影响。
      val stepLossSmoothed = fit.map { f =>
        val smoothSigma = math.max(1.0, cfg.resolutionM / 2.0 / spacingM)
        val smoothRadius = math.max(2, math.round(3 * smoothSigma).toInt)
        val ds = Dsp.gaussianSmooth(d, smoothRadius, smoothSigma)
        val plateauN = math.max(4, math.round(cfg.resolutionM * 4 / spacingM).toInt)
        val loPre = math.max(0, f.stepIdx - plateauN)
        val hiPre = f.stepIdx
        val loPost = f.stepIdx
        val hiPost = math.min(nI, f.stepIdx + plateauN)
        if (hiPre - loPre < 2 || hiPost - loPost < 2) f.stepDb
        else Stats.median(java.util.Arrays.copyOfRange(ds, loPost, hiPost)) -
          Stats.median(java.util.Arrays.copyOfRange(ds, loPre, hiPre))
      }
      if (fit.isDefined && enoughSpan) {
        val f = fit.get
        val noise = f.residualSigma
        val plateauShift = stepLossSmoothed.getOrElse(f.stepDb)
        // 注意：这里只用**最强的一个**台阶。若同一根纤上有两个台阶（例如一个熔接点
        // 温漂 + 一个窃听），单台阶模型只会保留更显著的那个 —— 这是已知限制，
        // 由"三波长配对"和"多纤关联"在外层补偿：熔接点温漂在各波长上都近似相等，
        // 而窃听弯曲在各波长上差异悬殊，外层能把两者分开。
        if (f.stepIdx > 0 && plateauShift < 0 && noise > 1e-9) {
          // 平台质量：台阶之后实测 D 相对"线性+台阶模型"的残差稳定性
          val after = java.util.Arrays.copyOfRange(d, f.stepIdx, d.length)
          val modelAfter = Array.tabulate(after.length)(k =>
            f.intercept + f.slope * (f.stepIdx + k) * spacingM + plateauShift)
          val residAfter = Array.tabulate(after.length)(k => after(k) - modelAfter(k))
          val q = 1.0 - math.min(1.0, Stats.robustSigma(residAfter) /
            (math.abs(plateauShift) + 1e-9))

          val persistence = persistenceAt(mat, nT, tau, f.stepIdx, spacingM)
          // z 必须用"台阶系数的标准误"，而不是单点残差 σ。
          //
          // 台阶系数是对整段（约 nI 个点）拟合出来的，其标准误 ≈ σ/√(有效独立点数)，
          // 比 σ 小一到两个数量级。直接用 σ 会把 z 系统性低估，导致 0.1–0.2 dB 的真实
          // 窃听全部漏检（实测：0.20dB 台阶只有 z=4.6，远低于门限）。
          //
          // 有效独立点数按空间相关长度折算：脉冲宽度把相邻采样点"抹平"，独立点数是
          // nI / (resolutionM/spacingM)。
          // nEff 必须从数据本身估：D 的残差里既有 OTDR 的空间相关噪声，
          // 也有"发射功率漂移、温度对衰减系数的影响"这类共模项 —— 后者相关长度可达
          // 公里级，绝不能按"分辨率单元"折算（那会高估独立点数，制造假警报）。
          //
          // 用残差的积分相关长度 L_int = 1 + 2Σρ(k) 直接给出：nEff = nI / L_int。
          val nEff = effectiveIndependentPoints(d, f, cfg)
          val z = -plateauShift / (noise / math.sqrt(math.max(1.0, nEff)))
          candidates += ((-plateauShift, math.abs(z), tau, persistence,
            math.max(0.0, q), f.stepIdx))
        }
      }
      tau += 1
    }

    if (candidates.isEmpty) return Vector.empty

    // 多重比较：候选数 = 独立分辨单元数 × τ 数
    val nResUnits = math.max(1, (nI * spacingM / cfg.resolutionM).toInt)
    val nTests = math.min(1000000L, math.max(1L, nResUnits.toLong * math.max(1, nT - 2 * cfg.minSegment)))
    val alphaAdj = Inference.sidakAlpha(cfg.familyAlpha, nTests.toInt)
    val zGate = Inference.zCritical(alphaAdj)
    if (System.getProperty("tp.debug") != null) {
      val top = candidates.sortBy(-_._2).take(3)
      println(f"    [debug] nTests=$nTests alphaAdj=${alphaAdj}%.3e zGate=${zGate}%.2f top=" +
        top.map(c => f"(loss=${c._1}%.3f z=${c._2}%.1f tau=${c._3} pers=${c._4}%.2f)").mkString(" "))
    }

    // 取过门限且"最深"的若干个，做空间-时间去重（同一位置/时刻只报一个）
    val ranked = candidates.filter { case (loss, z, _, pers, _, _) =>
      loss >= cfg.minLossDb && z >= zGate && pers >= cfg.minPersistence
    }.sortBy(-_._2)

    val out = scala.collection.mutable.ArrayBuffer.empty[SpatialEvent]
    ranked.foreach { case (loss, z, tau, pers, pq, eIdx) =>
      val edgeKm = kmAt(math.min(nI - 1, eIdx))
      val dup = out.exists(ev =>
        math.abs(ev.edgeKm - edgeKm) * 1000.0 < cfg.resolutionM * 2 &&
          math.abs(ev.tau - tau) <= 2)
      if (!dup) {
        val distSplice = if (span.splices.isEmpty) Double.PositiveInfinity
          else span.splices.map(s => math.abs(s.km - edgeKm) * 1000.0).min
        out += SpatialEvent(
          edgeKm = edgeKm,
          lossDb = loss,
          z = z,
          persistence = pers,
          tau = tau,
          onsetTime = sorted(usable(tau)).measuredAt,
          wavelengthNm = sorted.head.wavelengthNm,
          distToSpliceM = distSplice,
          plateauQuality = pq
        )
      }
    }
    out.toVector
  }

  /** 用 D 相对拟合模型的残差估计"有效独立点数"。
    *
    * 积分相关长度：L = 1 + 2·Σ_{k=1..K} ρ(k)，取到 ρ 首次转负或 K 上限。
    * 对白噪声 L=1；对强相关（如公里级漂移）L 会很大，约等于相关长度/采样间距。
    *
    * 性能：为控制开销，把序列**抽取**到约 2000 个点再算自相关（相关长度远大于
    * 抽取步长，抽取不改变积分相关长度的估计）。
    */
  private def effectiveIndependentPoints(d: Array[Double], f: StepFit, cfg: Config): Double = {
    val n = d.length
    if (n < 32) return 1.0
    val decim = math.max(1, n / 2000)
    val m = n / decim
    if (m < 16) return 1.0

    // 抽取后的残差（相对模型）
    val r = new Array[Double](m)
    var i = 0
    while (i < m) {
      val idx = i * decim
      val pred = f.intercept + f.slope * idx * (d.length.toDouble / n) + (if (idx >= f.stepIdx) f.stepDb else 0.0)
      r(i) = d(idx) - pred
      i += 1
    }
    val mean = Stats.mean(r)
    var var0 = 0.0
    i = 0
    while (i < m) { val v = r(i) - mean; var0 += v * v; i += 1 }
    var0 /= m
    if (var0 <= 1e-18) return 1.0

    val maxLag = math.min(m / 4, 400)
    var sum = 0.0
    var k = 1
    var stop = false
    while (k <= maxLag && !stop) {
      var s = 0.0
      var t = 0
      while (t + k < m) { s += (r(t) - mean) * (r(t + k) - mean); t += 1 }
      val rho = (s / (m - k)) / var0
      if (rho <= 0.0) stop = true else { sum += rho; k += 1 }
    }
    // 积分相关长度以"抽取后的采样"计，换算回原始采样间距
    val lIntDecimated = 1.0 + 2.0 * sum
    val lInt = lIntDecimated * decim
    math.max(1.0, n / math.max(1.0, lInt))
  }

  /** "线性 + 单台阶"联合拟合的结果。 */
  private final case class StepFit(stepIdx: Int, stepDb: Double, intercept: Double,
                                   slope: Double, residualSigma: Double)

  /** 在 D 上搜索最优 (台阶位置, 台阶高度, 线性项)。
    *
    * 用逐块粗搜索 + 局部细搜索。对固定 e，模型 D = a + b·x + ΔL·1[i≥e] 的最小二乘
    * 可以通过 3×3 正规方程 O(1) 求解 —— 正规方程各项都是关于 [1, x, 1[i≥e]] 的和，
    * 而 1[i≥e] 的交叉项只涉及"e 之后"的区间和，全部可用前缀和 O(1) 得到。
    * 否则每次 O(n) 会让整条缆的扫描慢到不可用（实测 30km×30 条 需 8 秒）。
    */
  private def fitLinearPlusStep(d: Array[Double], spacingM: Double,
                                cfg: Config): Option[StepFit] = {
    val n = d.length
    val coarse = math.max(2, math.round(cfg.resolutionM / 2.0 / spacingM).toInt)
    if (n < coarse * 8) return None

    // 前缀和：Σ1, Σx, Σx², Σy, Σxy, Σy²
    val s1 = new Array[Double](n + 1)
    val sx = new Array[Double](n + 1)
    val sxx = new Array[Double](n + 1)
    val sy = new Array[Double](n + 1)
    val sxy = new Array[Double](n + 1)
    val syy = new Array[Double](n + 1)
    var i = 0
    while (i < n) {
      val x = i * spacingM
      val y = d(i)
      s1(i + 1) = s1(i) + 1.0
      sx(i + 1) = sx(i) + x
      sxx(i + 1) = sxx(i) + x * x
      sy(i + 1) = sy(i) + y
      sxy(i + 1) = sxy(i) + x * y
      syy(i + 1) = syy(i) + y * y
      i += 1
    }
    @inline def rng(arr: Array[Double], from: Int): Double = arr(n) - arr(from)

    var bestE = -1
    var bestSse = Double.PositiveInfinity
    var bestCoef = Array(0.0, 0.0, 0.0)

    def evaluate(e: Int): Unit = {
      // 标签 z_i = 1[i≥e]，前缀和：Σ_{i≥e} w = rng(w, e)
      val n1 = rng(s1, e)         // 后段点数
      val n0 = n - n1             // 前段点数
      val sxe = rng(sx, e)
      val sx2e = rng(sxx, e)
      val sye = rng(sy, e)
      val sxye = rng(sxy, e)
      val sy2e = rng(syy, e)
      val sx0 = sx(n) - sxe
      val sx20 = sxx(n) - sx2e
      val sy0 = sy(n) - sye
      val sxy0 = sxy(n) - sxye
      val sy20 = syy(n) - sy2e
      val nx = sx(n)
      val nx2 = sxx(n)

      // 正规方程（对 [1, x, z] 的最小二乘）
      val A = Array(
        Array(n, nx, n1),
        Array(nx, nx2, sxe),
        Array(n1, sxe, n1)
      )
      val b = Array(sy(n), sxy(n), sye)
      val coef = solve3(A, b)
      if (coef != null) {
        // SSE = Σy² − βᵀ(Xᵀy)
        val sse = syy(n) - (coef(0) * sy(n) + coef(1) * sxy(n) + coef(2) * sye)
        if (sse < bestSse) { bestSse = sse; bestCoef = coef; bestE = e }
      }
    }

    var e = coarse
    while (e <= n - coarse) { evaluate(e); e += coarse }
    if (bestE < 0) return None

    var e2 = math.max(coarse, bestE - coarse)
    while (e2 <= math.min(n - coarse, bestE + coarse)) { evaluate(e2); e2 += 1 }

    val dof = math.max(1, n - 3)
    val sigma = math.sqrt(math.max(0.0, bestSse) / dof)
    Some(StepFit(bestE, bestCoef(2), bestCoef(0), bestCoef(1), sigma))
  }

  private def solve3(A: Array[Array[Double]], b: Array[Double]): Array[Double] = {
    val M = Array.tabulate(3, 4)((i, j) => if (j < 3) A(i)(j) else b(i))
    var col = 0
    while (col < 3) {
      var piv = col
      var r = col + 1
      while (r < 3) { if (math.abs(M(r)(col)) > math.abs(M(piv)(col))) piv = r; r += 1 }
      if (math.abs(M(piv)(col)) < 1e-12) return null
      val tmp = M(col); M(col) = M(piv); M(piv) = tmp
      var rr = 0
      while (rr < 3) {
        if (rr != col) {
          val f = M(rr)(col) / M(col)(col)
          var cc = col
          while (cc <= 3) { M(rr)(cc) -= f * M(col)(cc); cc += 1 }
        }
        rr += 1
      }
      col += 1
    }
    Array.tabulate(3)(i => M(i)(3) / M(i)(i))
  }

  /** 逐点中位数曲线。 */
  private def medianProfile(mat: Array[Array[Double]], idx: Array[Int], nI: Int): Array[Double] = {
    val out = new Array[Double](nI)
    val col = new Array[Double](idx.length)
    var i = 0
    while (i < nI) {
      var k = 0
      while (k < idx.length) { col(k) = mat(idx(k))(i); k += 1 }
      out(i) = Stats.median(col)
      i += 1
    }
    out
  }

  /** 去掉 D 的线性趋势（分块中位数 + Theil–Sen）。 */
  private def removeLinearTrend(d: Array[Double], spacingM: Double, cfg: Config): Array[Double] = {
    val blockM = math.max(cfg.resolutionM * 8, 250.0)
    val blockN = math.max(8, math.round(blockM / spacingM).toInt)
    val nBlocks = d.length / blockN
    if (nBlocks < 4) return d.clone()
    val xs = new Array[Double](nBlocks)
    val ys = new Array[Double](nBlocks)
    var b = 0
    while (b < nBlocks) {
      val lo = b * blockN
      xs(b) = (lo + blockN / 2) * spacingM
      ys(b) = Stats.median(java.util.Arrays.copyOfRange(d, lo, lo + blockN))
      b += 1
    }
    val (intercept, slope) = Stats.theilSen(xs, ys)
    if (slope.isNaN || intercept.isNaN) d.clone()
    else Array.tabulate(d.length)(i => d(i) - (intercept + slope * i * spacingM))
  }

  /** D[i] 的噪声尺度。
    *
    * 不能直接用二阶差分：二阶差分只对白噪声有已知换算系数（6σ²），而 OTDR 的空间
    * 噪声强自相关，换算系数完全不对（实测把 z 值抬高两个数量级）。
    *
    * 正确做法：把 D 分成若干**远大于相关长度**的块，用各块中位数的离散程度估噪声。
    * 因为块长 ≫ 相关长度，块中位数近似独立，这个估计是无偏的。
    *
    * 台阶只影响 1 个块，对块间 MAD 影响可忽略。
    */
  private def smoothNoiseLevel(sm: Array[Double], spacingM: Double, cfg: Config): Double = {
    val blockM = math.max(cfg.resolutionM * 4, 100.0)
    val blockN = math.max(4, math.round(blockM / spacingM).toInt)
    val nBlocks = sm.length / blockN
    if (nBlocks < 4) {
      val s = Stats.robustSigma(sm)
      return if (s.isNaN || s <= 1e-12) 1e-6 else s
    }
    val blockMedians = new Array[Double](nBlocks)
    var b = 0
    while (b < nBlocks) {
      val lo = b * blockN
      blockMedians(b) = Stats.median(java.util.Arrays.copyOfRange(sm, lo, lo + blockN))
      b += 1
    }
    // 块中位数自身的波动 ≈ σ/√blockN（对白噪声）；对相关噪声需按相关长度折算，
    // 但用块间 MAD 直接得到的"块中位数的离散度"本身就是要比较的量级：
    // 一个假台阶只让一个块偏移，块间 MAD 几乎不动。这里把它换算回"单块偏移量"尺度。
    val s = Stats.robustSigma(blockMedians)
    if (s.isNaN || s <= 1e-12) 1e-6 else math.max(s, 1e-6)
  }

  /** 在平滑差曲线上找最强的"向下阶跃"：扫描每个位置 e，比较其右侧平台与左侧平台的差。 */
  private def findStrongestDownStep(sm: Array[Double], plateN: Int): Option[(Int, Double)] = {
    val n = sm.length
    if (n < 2 * plateN + 2) return None
    // 前缀和，便于 O(1) 求任意区间均值
    val ps = new Array[Double](n + 1)
    var i = 0
    while (i < n) { ps(i + 1) = ps(i) + sm(i); i += 1 }
    def mean(lo: Int, hi: Int): Double = if (hi <= lo) 0.0 else (ps(hi) - ps(lo)) / (hi - lo)

    var best = -1
    var bestDrop = 0.0
    var e = plateN
    while (e <= n - plateN) {
      val left = mean(e - plateN, e)
      val right = mean(e, e + plateN)
      val drop = left - right    // 向下阶跃时为正值
      if (drop > bestDrop) { bestDrop = drop; best = e }
      e += 1
    }
    if (best < 0) None else Some((best, bestDrop))
  }

  /** 时间持续性：变点之后，各条后段曲线在边沿之后的电平是否都低于前段中位数。 */
  private def persistenceAt(mat: Array[Array[Double]], nT: Int,
                            tau: Int, eIdx: Int, spacingM: Double): Double = {
    val half = math.max(2, math.round(30.0 / spacingM).toInt)
    val lo = math.max(0, eIdx)
    val hi = math.min(mat(0).length, eIdx + half)
    if (hi <= lo || tau >= nT) return 0.0
    def segMean(row: Array[Double]): Double = {
      var s = 0.0
      var k = lo
      while (k < hi) { s += row(k); k += 1 }
      s / (hi - lo)
    }
    val preMeans = (0 until tau).map(t => segMean(mat(t))).toArray
    val postMeans = (tau until nT).map(t => segMean(mat(t))).toArray
    val base = Stats.median(preMeans)
    val postMed = Stats.median(postMeans)
    if (math.signum(postMed - base) == 0) 0.0
    else postMeans.count(m => math.signum(m - base) == math.signum(postMed - base)).toDouble /
      math.max(1, postMeans.length)
  }

  private def toMatrix(traces: Vector[OtdrTrace], usable: Array[Int])
      : (Array[Array[Double]], Array[Double], Double) = {
    val spacing = traces.map(_.spacingM).min
    val startKm = traces.map(_.startKm).min
    val maxKm = traces.map(t => t.startKm + (t.length - 1) * t.spacingM / 1000.0).max
    val n = math.max(4, math.round((maxKm - startKm) * 1000.0 / spacing).toInt + 1)
    val mat = new Array[Array[Double]](usable.length)
    var k = 0
    while (k < usable.length) {
      val tr = traces(usable(k))
      val row = new Array[Double](n)
      var i = 0
      while (i < n) {
        val km = startKm + i * spacing / 1000.0
        val pos = (km - tr.startKm) * 1000.0 / tr.spacingM
        row(i) =
          if (pos <= 0) tr.valuesDb(0)
          else if (pos >= tr.length - 1) tr.valuesDb(tr.length - 1)
          else {
            val lo = pos.toInt
            val f = pos - lo
            tr.valuesDb(lo) * (1 - f) + tr.valuesDb(lo + 1) * f
          }
        i += 1
      }
      mat(k) = row
      k += 1
    }
    val kmAt = Array.tabulate(n)(i => startKm + i * spacing / 1000.0)
    (mat, kmAt, spacing)
  }
}
