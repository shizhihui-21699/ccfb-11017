package tapdetect.sim

/** 光纤物理模型。
  *
  * 弯曲损耗是窃听检测里最重要的物理指纹：分光/弯折取光必然引入宏弯损耗，而宏弯损耗
  * 随波长陡增（1625nm ≫ 1550nm ≫ 1310nm）。熔接点的损耗漂移则近似与波长无关。
  * 因此"三波长差分"能把弯折窃听和温漂、老化区分开 —— 这是本系统的核心判据之一。
  */
object Physics {

  /** 宏弯损耗（dB）。
    *
    * ## 物理形式
    *
    * 单模光纤的宏弯损耗近似为
    *
    *     L ∝ exp(−k·(R − R_c(λ))) / √R        R_c 为临界弯曲半径，随波长增大
    *
    * 即：**在临界半径以下急剧上升，以上迅速消失**，且临界半径 ∝ λ。
    * 用"R/λ"直接进指数会得到荒谬的放大（本项目早期用 c·R/λ 的形式，
    * 结果 r=9mm 时算出 300 dB），因为真实模型里指数中是"到临界的距离"。
    *
    * 本实现用简化的"临界半径 + 指数衰减"：
    *
    *     L(R, λ) = A(λ)·exp(−(R − R_c(λ)) / w)     R > R_c 时按指数迅速归零
    *
    * 标定（G.652 量级）：R_c(1550)≈9.2 mm、R_c(1625)≈10 mm、R_c(1310)≈7.6 mm；
    * w ≈ 0.55 mm；A(1550)=0.25 dB/圈。于是同一弯曲下 1625nm 约为 1550nm 的 3.5 倍，
    * 1310nm 约为 1/100 —— 满足"三波长差分"的物理基础，且在 10–12 mm 半径下
    * 给出 0.01–0.5 dB 的**隐蔽**窃听量级。
    */
  def bendLossDb(radiusMm: Double, turns: Double, wavelengthNm: Int): Double = {
    val (amp, rc) = wavelengthNm match {
      case w if w >= 1625 => (0.42, 10.0)
      case w if w >= 1550 => (0.25, 9.2)
      case w if w >= 1490 => (0.20, 9.0)
      case _              => (0.06, 7.6)
    }
    val w = 0.55
    val l = amp * math.exp(-(radiusMm - rc) / w)
    math.max(0.0, turns * l)
  }

  /** 熔接点等效反射率对应的 OTDR 反射峰高度（dB，相对背向散射）。
    * 熔接接头基本无反射（<-70dB），机械接头/活动连接器有明显反射。
    */
  def reflectionHeightDb(mechanical: Boolean, poorQuality: Boolean): Double =
    if (mechanical) {
      if (poorQuality) 18.0 + 4.0 else 32.0
    } else if (poorQuality) 6.0 else 0.0

  /** 温度对熔接损耗的影响：劣质接头的温度系数大，且随盒内温度呈近似线性。 */
  def spliceLossAt(splice: tapdetect.model.Splice, boxTempC: Double, refTempC: Double,
                   yearsSinceCommission: Double): Double =
    splice.nominalLossDb +
      splice.tempSensitivityDbPerC * (boxTempC - refTempC) +
      splice.ageingDbPerYear * yearsSinceCommission

  /** 光纤群折射率随温度变化 ~7e-6/°C，导致整条 OTDR 曲线距离标定漂移（米级）。 */
  def distanceShiftM(lengthKm: Double, tempC: Double, refTempC: Double): Double =
    lengthKm * 1000.0 * 7e-6 * (tempC - refTempC)
}
