package tapdetect.core

import scala.collection.mutable.ArrayBuffer

/** 鲁棒统计工具。窃听检测的信噪比很低，所有估计都必须抗离群点：
  * 一条真实的光缆上有几十个熔接点、若干反射尖峰，它们都不应影响对"噪声水平"的估计。
  */
object Stats {

  /** 中位数。会就地排序输入副本，调用方不必担心副作用。 */
  def median(xs: Array[Double]): Double = {
    if (xs.isEmpty) return Double.NaN
    val a = xs.clone()
    java.util.Arrays.sort(a)
    val n = a.length
    if (n % 2 == 1) a(n / 2) else 0.5 * (a(n / 2 - 1) + a(n / 2))
  }

  def quantile(xs: Array[Double], q: Double): Double = {
    if (xs.isEmpty) return Double.NaN
    val a = xs.filterNot(_.isNaN).clone()
    java.util.Arrays.sort(a)
    if (a.isEmpty) return Double.NaN
    val pos = q * (a.length - 1)
    val lo = math.floor(pos).toInt
    val hi = math.min(lo + 1, a.length - 1)
    a(lo) + (pos - lo) * (a(hi) - a(lo))
  }

  /** MAD → 标准差的一致估计（正态假设下 σ ≈ 1.4826·MAD）。 */
  def robustSigma(xs: Array[Double]): Double = {
    val m = median(xs)
    if (m.isNaN) return Double.NaN
    val dev = new Array[Double](xs.length)
    var i = 0
    while (i < xs.length) { dev(i) = math.abs(xs(i) - m); i += 1 }
    1.4826 * median(dev)
  }

  def mean(xs: Array[Double]): Double = {
    if (xs.isEmpty) return Double.NaN
    var s = 0.0
    var i = 0
    while (i < xs.length) { s += xs(i); i += 1 }
    s / xs.length
  }

  def std(xs: Array[Double]): Double = {
    val m = mean(xs)
    if (m.isNaN) return Double.NaN
    var s = 0.0
    var i = 0
    while (i < xs.length) { val d = xs(i) - m; s += d * d; i += 1 }
    math.sqrt(s / math.max(1, xs.length - 1))
  }

  /** 稳健线性回归（Theil–Sen）：两两斜率的中位数，对离群点不敏感。
    *
    * 代价 O(n²)，因此只用于小样本 —— 本项目里用于：
    *   - 曲线衰减斜率估计（分块后只有几百个块）；
    *   - 段差上"线性趋势 + 台阶"初筛的趋势项。
    */
  def theilSen(x: Array[Double], y: Array[Double]): (Double, Double) = {
    val n = math.min(x.length, y.length)
    if (n < 2) return (Double.NaN, Double.NaN)
    val slopes = new ArrayBuffer[Double](n * (n - 1) / 2)
    var i = 0
    while (i < n) {
      var j = i + 1
      while (j < n) {
        val dx = x(j) - x(i)
        if (math.abs(dx) > 1e-9) slopes += (y(j) - y(i)) / dx
        j += 1
      }
      i += 1
    }
    if (slopes.isEmpty) return (mean(java.util.Arrays.copyOf(y, n)), 0.0)
    val b = median(slopes.toArray)
    val intercepts = new Array[Double](n)
    var k = 0
    while (k < n) { intercepts(k) = y(k) - b * x(k); k += 1 }
    (median(intercepts), b)
  }
}
