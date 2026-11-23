package tapdetect.sim

import java.time.Instant
import tapdetect.model.EnvSample

/** 环境过程模型：温度、施工活动、老化。
  *
  * 为什么需要它：真实系统里我们不能直接看到"环境噪声有多大"，但可以把温度、
  * 施工工单、历史曲线当成回归量去解释观测到的变化。仿真器里显式地生成这些量，
  * 检测器则只能看到观测量 —— 这样评估才是诚实的。
  */
object Env {

  /** OU 过程的相关时间（秒）→ 回归速率 θ。 */
  @inline def thetaFor(correlationSeconds: Double): Double = 1.0 / correlationSeconds

  /** 给定相关时间与稳态标准差，反算 OU 的扩散强度 σ = std·√(2θ)。 */
  @inline def sigmaFor(std: Double, theta: Double): Double = std * math.sqrt(2.0 * theta)

  /** 日温周期的相位随机化，避免所有缆段同相。 */
  def diurnal(at: Instant, baseC: Double, ampC: Double, phaseHours: Double): Double = {
    val h = at.toEpochMilli / 3600000.0
    baseC + ampC * math.sin(2 * math.Pi * (h + phaseHours) / 24.0)
  }

  /** 季节温度项（正弦年周期）。 */
  def seasonal(at: Instant, baseC: Double, ampC: Double, phaseDays: Double): Double = {
    val d = at.toEpochMilli / 86400000.0
    baseC + ampC * math.sin(2 * math.Pi * (d + phaseDays) / 365.25)
  }

  /** 带红噪声（Ornstein–Uhlenbeck）的随机游走。返回长度 n 的序列。
    *
    * OU 过程：dx = θ(μ − x)dt + σ dW
    *
    * 必须用**精确离散化**（AR(1) 形式），不能用 Euler 一阶近似：
    *   x_{k+1} = μ + (x_k − μ)e^{−θΔt} + σ√((1 − e^{−2θΔt})/(2θ))·ε
    * Euler 形式在 θΔt > 1 时（比如 θ=0.02/s、Δt=300s）会得到 |1−θΔt| = 5 > 1 的
    * 递推系数，序列指数发散。这个坑在本项目里真实踩过 —— 盒内温度涨到 76°C。
    *
    * @param theta 均值回归速率（1/秒），相关时间 τ = 1/θ
    * @param sigma 扩散强度；稳态标准差 = σ/√(2θ)
    */
  def ouProcess(rng: java.util.Random, n: Int, mean: Double, theta: Double,
                sigma: Double, dt: Double, x0: Double = Double.NaN): Array[Double] = {
    val out = new Array[Double](n)
    var x = if (x0.isNaN) mean else x0
    val decay = math.exp(-theta * dt)
    val noiseScale = sigma * math.sqrt((1.0 - decay * decay) / (2.0 * theta))
    var i = 0
    while (i < n) {
      x = mean + (x - mean) * decay + noiseScale * rng.nextGaussian()
      out(i) = x
      i += 1
    }
    out
  }
}
