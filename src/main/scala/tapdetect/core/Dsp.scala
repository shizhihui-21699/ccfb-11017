package tapdetect.core

/** 数字信号处理原语。
  *
  * 纯 Scala 数组实现，无外部依赖：处理的是一张网每天数十亿个采样点，
  * 需要避免装箱，也希望现场部署时是单 jar。
  *
  * 只保留实际用到的算子。基准测试表明其余算子（交叉相关配准、极值抑制、
  * 滑动标准差）在本系统的判据体系里用不上 —— 配准由"共同距离栅格"解决，
  * 极值抑制由"阶梯模型拟合"替代，滑动标准差由"分块中位数"替代。
  */
object Dsp {

  /** 一维 "same" 模式卷积：out(i) = Σ_k kernel(k)·x(i − k + radius)。 */
  def convolveSame(x: Array[Double], kernel: Array[Double]): Array[Double] = {
    val n = x.length
    val k = kernel.length
    val r = k / 2
    val out = new Array[Double](n)
    var i = 0
    while (i < n) {
      var acc = 0.0
      var j = 0
      while (j < k) {
        val idx = i + j - r
        if (idx >= 0 && idx < n) acc += kernel(j) * x(idx)
        j += 1
      }
      out(i) = acc
      i += 1
    }
    out
  }

  /** 归一化高斯核（半径 radius 个采样）。 */
  def gaussianKernel(radius: Int, sigma: Double): Array[Double] = {
    val k = new Array[Double](2 * radius + 1)
    var i = -radius
    var s = 0.0
    while (i <= radius) {
      val v = math.exp(-0.5 * (i * i) / (sigma * sigma))
      k(i + radius) = v; s += v
      i += 1
    }
    var j = 0
    while (j < k.length) { k(j) /= s; j += 1 }
    k
  }

  /** 高斯平滑。空间尺度取物理分辨率的一半（见 ChangePointScanner）。 */
  def gaussianSmooth(x: Array[Double], radius: Int, sigma: Double): Array[Double] =
    if (radius <= 0) x.clone() else convolveSame(x, gaussianKernel(radius, sigma))

  /** 二阶差分：d2(i) = x(i+1) − 2x(i) + x(i−1)。用于对线性趋势和台阶都不敏感的噪声刻画。 */
  def diff2(x: Array[Double]): Array[Double] = {
    val n = x.length
    val out = new Array[Double](n)
    var i = 1
    while (i < n - 1) { out(i) = x(i + 1) - 2 * x(i) + x(i - 1); i += 1 }
    out
  }
}
