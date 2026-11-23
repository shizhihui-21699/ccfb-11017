package tapdetect.model

/** 光缆段台账中的最小可派工单元（通常对应一"盘"光缆 / GIS 中的一个缆段）。 */
final case class CableSection(
    drumNo: Int,
    startKm: Double,
    endKm: Double,
    fromNode: String,
    toNode: String
)

/** 接头盒（窃听者最常下手的位置：开盒、盘纤、加分光器都在这里完成）。 */
final case class Closure(
    id: String,
    boxNo: String,
    km: Double,
    kind: String = "直埋接头盒"
)

/** 熔接点 / 连接点。
  *
  * @param nominalLossDb         竣工时的单向损耗
  * @param tempSensitivityDbPerC 损耗随温度的变化系数。劣质机械接头可达 0.02 dB/°C，
  *                              是温度造成 OTDR "假阶跃"的主要来源之一。
  * @param ageingDbPerYear       老化漂移速率（长期趋势）
  * @param mechanical            true 表示机械接头/活动连接器（有菲涅尔反射，OTDR 可见反射峰）
  */
final case class Splice(
    km: Double,
    closureId: Option[String],
    nominalLossDb: Double,
    tempSensitivityDbPerC: Double = 0.002,
    ageingDbPerYear: Double = 0.005,
    mechanical: Boolean = false
)

/** 一条被监测的光纤通道（两端的 OTDR/OPM 之间）。
  *
  * @param cableId  物理光缆编号。同一光缆上的多根纤芯共享缆径和接头盒 —— 多纤同时出现
  *                 同址异常，几乎可以断定是接头盒被人打开（物理入侵），而不是单纤被定向窃听。
  * @param fiberNo  纤芯序号
  */
final case class Span(
    id: String,
    label: String,
    cableId: String,
    fiberNo: Int,
    lengthKm: Double,
    sections: Vector[CableSection],
    closures: Vector[Closure],
    splices: Vector[Splice],
    wavelengths: Vector[Int] = Vector(1310, 1550, 1625),
    primaryWavelengthNm: Int = 1550,
    groupIndex: Double = 1.468,
    attenuationDbPerKm: Map[Int, Double] = Map(1310 -> 0.34, 1550 -> 0.20, 1625 -> 0.23),
    /** OTDR 发射端所在端（A/B），影响"哪一端能看到该事件"的判断 */
    otdrEnd: String = "A"
) {
  require(lengthKm > 0, s"span $id: length must be positive")
  require(splices.forall(s => s.km > 0 && s.km < lengthKm), s"span $id: splice outside span")

  def sectionAt(km: Double): Option[CableSection] =
    sections.find(s => km >= s.startKm && km <= s.endKm)

  def spliceNear(km: Double, tolM: Double): Option[Splice] =
    splices.filter(s => math.abs(s.km - km) * 1000.0 <= tolM)
      .sortBy(s => math.abs(s.km - km)).headOption

  def closureNear(km: Double, tolM: Double): Option[Closure] =
    closures.filter(c => math.abs(c.km - km) * 1000.0 <= tolM)
      .sortBy(c => math.abs(c.km - km)).headOption

  def nearestClosure(km: Double): Option[(Closure, Double)] =
    closures.map(c => c -> ((km - c.km) * 1000.0)).sortBy(p => math.abs(p._2)).headOption

  def closureById(id: String): Option[Closure] = closures.find(_.id == id)

  /** 给现场人员的可执行描述："第7盘（12.00–16.00km），距 12#接头盒 +87m，搜索范围 ±30m"。 */
  def describe(km: Double, windowM: Double): String = {
    val sec = sectionAt(km).map { s =>
      f"第${s.drumNo}盘（${s.startKm}%.2f–${s.endKm}%.2f km，${s.fromNode}→${s.toNode}）"
    }.getOrElse("（缆段台账未覆盖）")
    val clo = nearestClosure(km) match {
      case Some((c, off)) if math.abs(off) <= 5000 =>
        val dir = if (off >= 0) "+" else "−"
        f"，距 ${c.boxNo} 接头盒 $dir${math.abs(off).round}m"
      case _ => ""
    }
    f"$sec${clo}，搜索范围 ±${windowM.round} m"
  }
}
