package tapdetect.core

/** 统计推断：稳健回归、假设检验、多重比较校正。
  *
  * 全部是手工实现的闭式解，不依赖任何统计库：
  *  - IRLS + Huber 权，用于"温度 → 阶跃量"的稳健回归（少量离群点不会带偏系数）
  *  - CUSUM 均值漂移检验，用于在时间轴上找窃听的"起始时刻"
  *  - Šidák 校正，用于"成千上万条线路 × 上万个距离单元"的多重比较
  */
object Inference {

  private val sqrt2 = math.sqrt(2.0)

  /** Abramowitz–Stegun 7.1.26 误差函数近似，绝对误差 < 1.5e-7。 */
  def erf(x: Double): Double = {
    val sign = if (x < 0) -1.0 else 1.0
    val ax = math.abs(x)
    val t = 1.0 / (1.0 + 0.3275911 * ax)
    val y = 1.0 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t -
      0.284496736) * t + 0.254829592) * t * math.exp(-ax * ax)
    sign * y
  }

  /** 标准正态 CDF。 */
  def normCdf(z: Double): Double = 0.5 * (1.0 + erf(z / sqrt2))

  /** 双侧 p 值。 */
  def twoSidedP(z: Double): Double = 2.0 * (1.0 - normCdf(math.abs(z)))

  /** 双侧 z 临界值：P(|Z| > z) = alpha。用二分法反解。 */
  def zCritical(alpha: Double): Double = {
    val target = 1.0 - alpha / 2.0
    var lo = 0.0
    var hi = 40.0
    var i = 0
    while (i < 200) {
      val mid = 0.5 * (lo + hi)
      if (normCdf(mid) < target) lo = mid else hi = mid
      i += 1
    }
    0.5 * (lo + hi)
  }

  /** Šidák 校正：m 次独立检验下，控制族错误率 alpha 所需的单次显著性水平。 */
  def sidakAlpha(alphaFamily: Double, m: Int): Double =
    if (m <= 1) alphaFamily else 1.0 - math.pow(1.0 - alphaFamily, 1.0 / m)

  /** 稳健 z 分数：用中位数和 MAD，避免异常点污染。 */
  def robustZ(x: Double, center: Double, sigma: Double): Double =
    if (sigma <= 1e-12) {
      if (math.abs(x - center) < 1e-12) 0.0 else math.signum(x - center) * 1e9
    } else (x - center) / sigma

  final case class LinearModel(intercept: Double, slopes: Array[Double], sigma: Double,
                               weights: Array[Double]) {
    def predict(xs: Array[Double]): Double = {
      var v = intercept
      var i = 0
      while (i < slopes.length) { v += slopes(i) * xs(i); i += 1 }
      v
    }
    def params: Array[Double] = intercept +: slopes
  }

  /** Huber 权重的 IRLS 稳健多元线性回归。设计矩阵 X 按列存储（每列长度 n）。 */
  def huberRegression(X: Array[Array[Double]], y: Array[Double],
                      maxIter: Int = 30, c: Double = 1.345): LinearModel = {
    val n = y.length
    val p = X.length
    require(X.forall(_.length == n), "IRLS: column length mismatch")
    val w = Array.fill(n)(1.0)
    var beta = Array.fill(p + 1)(0.0)
    var sigma = 1.0
    var iter = 0
    var converged = false
    // 初值：普通最小二乘（加权）
    while (iter < maxIter && !converged) {
      val (b, resid) = wls(X, y, w)
      beta = b
      // 稳健尺度：加权残差的 MAD
      val absR = resid.map(math.abs)
      sigma = 1.4826 * Stats.median(absR)
      if (sigma < 1e-12) sigma = 1e-12
      val newW = new Array[Double](n)
      var i = 0
      var maxDelta = 0.0
      while (i < n) {
        val u = math.abs(resid(i)) / sigma
        val wi = if (u <= c) 1.0 else c / u
        newW(i) = wi
        maxDelta = math.max(maxDelta, math.abs(wi - w(i)))
        i += 1
      }
      w.indices.foreach(i => w(i) = newW(i))
      converged = maxDelta < 1e-4
      iter += 1
    }
    val (bFinal, _) = wls(X, y, w)
    beta = bFinal
    LinearModel(beta(0), beta.drop(1), sigma, w)
  }

  /** 加权最小二乘：解 (XᵀWX)β = XᵀWy，含截距列。返回 (β, 残差)。 */
  private def wls(X: Array[Array[Double]], y: Array[Double], w: Array[Double])
      : (Array[Double], Array[Double]) = {
    val n = y.length
    val p = X.length + 1 // +截距
    val XtWX = Array.fill(p, p)(0.0)
    val XtWy = Array.fill(p)(0.0)
    var i = 0
    while (i < n) {
      val wi = w(i)
      val xi = new Array[Double](p)
      xi(0) = 1.0
      var j = 0
      while (j < p - 1) { xi(j + 1) = X(j)(i); j += 1 }
      var a = 0
      while (a < p) {
        XtWy(a) += wi * xi(a) * y(i)
        var b = 0
        while (b < p) { XtWX(a)(b) += wi * xi(a) * xi(b); b += 1 }
        a += 1
      }
      i += 1
    }
    val beta = solveSPD(XtWX, XtWy)
    val resid = new Array[Double](n)
    i = 0
    while (i < n) {
      var pred = beta(0)
      var j = 0
      while (j < p - 1) { pred += beta(j + 1) * X(j)(i); j += 1 }
      resid(i) = y(i) - pred
      i += 1
    }
    (beta, resid)
  }

  /** 高斯消元（部分主元）解小型对称正定方程组；p 最大到 ~6，够用。 */
  private def solveSPD(A: Array[Array[Double]], b: Array[Double]): Array[Double] = {
    val p = b.length
    val M = Array.tabulate(p, p + 1)((i, j) => if (j < p) A(i)(j) else b(i))
    var col = 0
    while (col < p) {
      var piv = col
      var r = col + 1
      while (r < p) { if (math.abs(M(r)(col)) > math.abs(M(piv)(col))) piv = r; r += 1 }
      if (math.abs(M(piv)(col)) < 1e-15) {
        // 秩亏：退回零解（调用方会用 sigma 兜底）
        return Array.fill(p)(0.0)
      }
      val tmp = M(col); M(col) = M(piv); M(piv) = tmp
      var rr = 0
      while (rr < p) {
        if (rr != col) {
          val f = M(rr)(col) / M(col)(col)
          var cc = col
          while (cc <= p) { M(rr)(cc) -= f * M(col)(cc); cc += 1 }
        }
        rr += 1
      }
      col += 1
    }
    Array.tabulate(p)(i => M(i)(p) / M(i)(i))
  }

  // ---------------------------------------------------------------------------
  // 两样本与序贯检验
  // ---------------------------------------------------------------------------

  /** Mann–Whitney U（双侧正态近似），返回 (z, p)。对分布形状不敏感。
    *
    * 并列处理：取平均秩，并在方差里做并列校正；否则在"大量取整值"的数据上会把 p 值算小。
    */
  def mannWhitney(a: Array[Double], b: Array[Double]): (Double, Double) = {
    val na = a.length; val nb = b.length
    if (na < 2 || nb < 2) return (0.0, 1.0)
    val all = new Array[Double](na + nb)
    Array.copy(a, 0, all, 0, na)
    Array.copy(b, 0, all, na, nb)
    val idx = all.indices.sortBy(all(_))
    val ranks = new Array[Double](na + nb)
    var tieCorrection = 0.0
    var i = 0
    while (i < idx.length) {
      var j = i
      while (j + 1 < idx.length && math.abs(all(idx(j + 1)) - all(idx(i))) < 1e-12) j += 1
      val avgRank = (i + j + 2) / 2.0 // 1-based，秩 i+1..j+1 的平均
      var k = i
      while (k <= j) { ranks(idx(k)) = avgRank; k += 1 }
      val tieSize = (j - i + 1).toDouble
      if (tieSize > 1) tieCorrection += tieSize * tieSize * tieSize - tieSize
      i = j + 1
    }
    var r1 = 0.0
    i = 0
    while (i < na) { r1 += ranks(i); i += 1 }
    val U = r1 - na * (na + 1) / 2.0
    val muU = na.toDouble * nb / 2.0
    val nTotal = (na + nb).toDouble
    val sigmaU = math.sqrt(na.toDouble * nb / 12.0 * ((nTotal + 1) - tieCorrection / (nTotal * (nTotal - 1))))
    val z = if (sigmaU <= 1e-12) 0.0 else (U - muU) / sigmaU
    (z, twoSidedP(z))
  }

  final case class ChangePointResult(
      onsetIndex: Int,
      shift: Double,
      zScore: Double,
      pValue: Double,
      preMedian: Double,
      postMedian: Double,
      /** 检测到偏移之后仍保持的稳定程度（0..1），用于区分"持续性窃听"和"一次性施工扰动" */
      persistence: Double
  )

  /** 基于相邻一阶差分的稳健 σ 估计（Rousseeuw & Croux 的 Sn 的简化版）。
    *
    * 为什么不能用 MAD(x)：如果序列里有一半样本已经平移（正是我们要检测的情形），
    * MAD 会随台阶高度一起变大，把 z 值压低到检不出来。而相邻差分 |x[i+1]−x[i]|
    * 在台阶之外都很小，只有台阶那一个点会变大 —— 中位数因此几乎不受台阶影响。
    */
  def robustSigmaFromDiffs(x: Array[Double]): Double = {
    val n = x.length
    if (n < 3) return Double.NaN
    val d = new Array[Double](n - 1)
    var i = 0
    while (i < n - 1) { d(i) = math.abs(x(i + 1) - x(i)); i += 1 }
    // 若 x_i = μ + σ·ε_i（ε ~ N(0,1)），则 d_i = |x_{i+1} − x_i| 是半正态，其尺度为 √2·σ，
    // 中位数为 √2·σ·Φ⁻¹(0.75) ≈ 0.9539σ。所以 σ̂ = median(d) / 0.9539。
    Stats.median(d) / 0.9539
  }

  /** 在残差序列上做 CUSUM 均值漂移检验，返回最显著的变点。
    *
    * 适用于：温度变化被回归掉之后，窃听造成的残余偏移表现为一个持久的均值台阶。
    * 对"施工震动"这种一次性脉冲，persistence 会很低，从而被降权。
    *
    * @param minSegment 变点两侧各自至少需要的样本数
    */
  def detectChangePoint(resid: Array[Double], minSegment: Int = 5): ChangePointResult = {
    val n = resid.length
    if (n < 2 * minSegment) {
      return ChangePointResult(0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0)
    }
    val sigmaDiff = robustSigmaFromDiffs(resid)
    val sigma = if (sigmaDiff.isNaN || sigmaDiff <= 1e-12) math.max(1e-12, Stats.robustSigma(resid))
                else sigmaDiff
    var bestTau = -1
    var bestZ = 0.0
    var bestShift = 0.0
    var tau = minSegment
    while (tau <= n - minSegment) {
      // 约定：pre = [0, tau)，post = [tau, n)。tau 即"第一个变化样本"的下标。
      val pre = java.util.Arrays.copyOfRange(resid, 0, tau)
      val post = java.util.Arrays.copyOfRange(resid, tau, n)
      val mPre = Stats.median(pre)
      val mPost = Stats.median(post)
      // 中位数差的稳健 z：方差 ≈ (π/2)σ²/n
      val se = sigma * math.sqrt((math.Pi / 2.0) * (1.0 / tau + 1.0 / (n - tau)))
      val z = if (se <= 1e-15) 0.0 else (mPost - mPre) / se
      // 若 z 与当前最优几乎相同（差 <0.3%），取更早的变点 —— 起始时刻宁早勿晚，
      // 现场"第一次出现"的时间对判断窃听上线时间更有价值。
      val materiallyBetter = math.abs(z) > math.abs(bestZ) * 1.003
      if (bestTau < 0 || materiallyBetter) {
        bestZ = z; bestTau = tau; bestShift = mPost - mPre
      }
      tau += 1
    }
    if (bestTau < 0) return ChangePointResult(0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0)
    val pre = java.util.Arrays.copyOfRange(resid, 0, bestTau)
    val post = java.util.Arrays.copyOfRange(resid, bestTau, n)
    // 持续性：变点之后"与 pre 中位数偏离同号"的样本占比；施工脉冲会随时间衰减
    val mPre = Stats.median(pre)
    val mPost = Stats.median(post)
    val sameSign =
      if (math.signum(mPost - mPre) == 0) 0.0
      else post.count(x => math.signum(x - mPre) == math.signum(mPost - mPre)).toDouble / post.length
    // 后段自身的稳定性（越稳定越像持续窃听）
    val postSigma = Stats.robustSigma(post)
    val stability = 1.0 / (1.0 + postSigma / (math.abs(mPost - mPre) + 1e-9))
    ChangePointResult(bestTau, bestShift, bestZ, twoSidedP(bestZ), mPre, mPost,
      persistence = sameSign * stability)
  }
}
