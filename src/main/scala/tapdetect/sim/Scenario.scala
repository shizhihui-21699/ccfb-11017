package tapdetect.sim

import java.time.{Duration, Instant}
import tapdetect.model._

/** 确定性测试场景：一个接头、一根纤、一个已知的弯折窃听。
  * 测试和演示都用它，避免"随机种子恰好没有真值"这类不可复现的问题。
  */
object Scenario {

  /** 一段 30km 的光缆，熔接点每 4km 一个，窃听点位于 15.6km（距 16km 接头盒 −400m）。 */
  def singleTap(
      tapKm: Double = 15.6,
      excessLossDb: Double = 0.15,
      tapStartDay: Int = 10,
      days: Int = 30,
      noiseLevel: Double = 1.0
  ): World = {
    val start = Instant.parse("2024-01-01T00:00:00Z")
    val splicesKm = (1 to 7).map(_ * 4.0).toVector
    val splices = splicesKm.map { km =>
      Splice(km, Some(s"J-${(km / 4).toInt}"), 0.05, 0.001, 0.005, mechanical = false)
    }
    val closures = splicesKm.zipWithIndex.map { case (km, i) =>
      Closure(s"J-${i + 1}", s"J-${i + 2}", km, "直埋接头盒")
    }
    val sections = (0 until 8).map { i =>
      CableSection(i + 1, i * 3.75, (i + 1) * 3.75, s"J-$i", s"J-${i + 1}")
    }.toVector
    val span = Span(
      id = "T-1", label = "测试段", cableId = "TC-1", fiberNo = 1,
      lengthKm = 30.0, sections = sections, closures = closures, splices = splices
    )
    val tap = TapEvent("TAP-1", span.id, tapKm, excessLossDb, 9.0, 2.0,
      start.plus(Duration.ofDays(tapStartDay.toLong)).plus(Duration.ofHours(2)))
    val env = Simulator.makeEnvPublic(new java.util.Random(11L),
      Simulator.Config(seed = 11L, days = days, spanCount = 1, fibersPerCable = 1),
      span, noiseLevel)
    World(Vector(span), Vector(tap), env, start)
  }

  /** 无窃听的控制组：同一条缆，无事件。 */
  def noTap(days: Int = 30): World = {
    val w = singleTap(days = days)
    w.copy(taps = Vector.empty)
  }
}
