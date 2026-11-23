package tapdetect

import java.time.Duration
import java.util.Random
import tapdetect.core.{Inference, Stats}
import tapdetect.engine.{ChangePointScanner, EnvIndex, WavelengthAnalyzer}
import tapdetect.sim._

class ScenarioSuite extends munit.FunSuite {

  private def traces(world: World, span: tapdetect.model.Span, wl: Int, days: Int,
                     seed: Long = 5L, spacingM: Double = 1.0): Vector[tapdetect.model.OtdrTrace] = {
    val rng = new Random(seed)
    val envIdx = EnvIndex(world.envFor(span.id))
    (0 until days).map { d =>
      val at = world.start.plus(Duration.ofDays(d.toLong)).plus(Duration.ofHours(23))
      Simulator.otdrTrace(world, span, at, wl, rng, 20.0, spacingM, envIdx)
    }.toVector
  }

  test("OTDR 曲线量级正确：30km/1550nm 总损耗约 6dB") {
    val w = Scenario.singleTap()
    val sp = w.spans.head
    val t = traces(w, sp, 1550, 3).head
    val drop = t.valuesDb.head - t.valuesDb.last
    assert(drop > 4.5 && drop < 8.0, s"unexpected loss $drop")
  }

  test("检出 0.15dB 窃听台阶并定位到 15.6km") {
    val w = Scenario.singleTap(excessLossDb = 0.15, tapStartDay = 10)
    val sp = w.spans.head
    val envIdx = EnvIndex(w.envFor(sp.id))
    val ts = traces(w, sp, 1550, 30)
    val evs = ChangePointScanner.scan(sp, ts, envIdx, ChangePointScanner.Config())
    val hit = evs.find(e => math.abs(e.edgeKm - 15.6) * 1000.0 < 100.0)
    assert(hit.isDefined, s"no event near 15.6km; got ${evs.map(e => (e.edgeKm, e.lossDb))}")
    val e = hit.get
    assert(math.abs(e.lossDb - 0.15) < 0.06, s"loss ${e.lossDb}")
    assert(e.persistence >= 0.6, s"persistence ${e.persistence}")
  }

  test("无窃听控制组不产生事件") {
    val w = Scenario.noTap()
    val sp = w.spans.head
    val envIdx = EnvIndex(w.envFor(sp.id))
    var hits = 0
    for (seed <- 0 until 3) {
      val ts = traces(w, sp, 1550, 30, seed = seed.toLong * 1000 + 11)
      hits += ChangePointScanner.scan(sp, ts, envIdx, ChangePointScanner.Config()).length
    }
    assertEquals(hits, 0, s"false positives across 3 seeds: $hits")
  }

  test("三波长：弯曲判定为 bend-like，平损耗判定为 wavelength-flat") {
    val bend = Map(1310 -> 0.002, 1550 -> 0.15, 1625 -> 0.52)
    val flat = Map(1310 -> 0.15, 1550 -> 0.15, 1625 -> 0.15)
    assertEquals(WavelengthAnalyzer.classify(bend).verdict, "bend-like")
    assertEquals(WavelengthAnalyzer.classify(flat).verdict, "wavelength-flat")
  }

  test("宏弯损耗物理：临界半径以下陡增，1625nm 明显大于 1550nm，1310nm 几乎为零") {
    // R=10mm 是"隐蔽窃听"的典型弯曲半径，1550nm 单圈约 0.06 dB
    val l10_1550 = tapdetect.sim.Physics.bendLossDb(10.0, 1.0, 1550)
    assert(l10_1550 > 0.02 && l10_1550 < 0.15, s"R=10mm/1550nm loss $l10_1550")
    // 缠 2 圈 → 约 2 倍
    val l10_2 = tapdetect.sim.Physics.bendLossDb(10.0, 2.0, 1550)
    assert(math.abs(l10_2 / l10_1550 - 2.0) < 0.01, s"turns scaling ${l10_2 / l10_1550}")
    // 半径减到 8mm，损耗陡增一个数量级以上
    val l8_1550 = tapdetect.sim.Physics.bendLossDb(8.0, 1.0, 1550)
    assert(l8_1550 > 10 * l10_1550, s"R=8mm loss $l8_1550 vs R=10mm $l10_1550")
    // 三波长比：同一弯曲下 1625 ≫ 1550 ≫ 1310 —— 这是检测判据的物理基础
    val l1625 = tapdetect.sim.Physics.bendLossDb(10.5, 2.0, 1625)
    val l1550 = tapdetect.sim.Physics.bendLossDb(10.5, 2.0, 1550)
    val l1310 = tapdetect.sim.Physics.bendLossDb(10.5, 2.0, 1310)
    assert(l1625 > 2 * l1550, s"1625/1550 = ${l1625 / l1550}")
    assert(l1310 < 0.05 * l1550, s"1310/1550 = ${l1310 / l1550}")
  }

  test("CUSUM 变点定位台阶起点") {
    val rng = new Random(1L)
    val n = 30
    val x = Array.tabulate(n)(i => if (i < 12) rng.nextGaussian() * 0.02 else -0.2 + rng.nextGaussian() * 0.02)
    val cp = Inference.detectChangePoint(x, 5)
    assert(math.abs(cp.onsetIndex - 12) <= 1, s"onset ${cp.onsetIndex}")
    assert(cp.shift < -0.15 && cp.shift > -0.25, s"shift ${cp.shift}")
  }

  test("Mann-Whitney 秩和检验") {
    val a = Array.tabulate(20)(i => i * 0.01)
    val b = Array.tabulate(20)(i => 1.0 + i * 0.01)
    val (z, p) = Inference.mannWhitney(a, b)
    assert(z < -3.0 && p < 0.01)
  }

  test("Huber 稳健回归不被单个离群点带偏") {
    val rng = new Random(3L)
    val xs = Array.tabulate(40)(i => 10.0 + i * 0.5)
    val ys = xs.map(x => 2.0 + 0.1 * x + rng.nextGaussian() * 0.01)
    ys(20) += 5.0
    val m = Inference.huberRegression(Array(xs), ys)
    assert(math.abs(m.slopes(0) - 0.1) < 0.01, s"slope ${m.slopes(0)}")
    assert(math.abs(m.intercept - 2.0) < 0.2, s"intercept ${m.intercept}")
  }

  test("OU 过程使用精确离散化，不发散、尺度正确") {
    val rng = new Random(1L)
    val theta = Env.thetaFor(4 * 86400.0)
    val xs = Env.ouProcess(rng, 20000, 0.0, theta, Env.sigmaFor(1.2, theta), 300.0)
    val sigma = Stats.std(xs)
    assert(sigma > 0.85 && sigma < 1.4, s"sigma=$sigma")
    assert(xs.forall(x => math.abs(x) < 8.0))
  }

  test("施工震动曲线被剔除后仍能检出") {
    val w = Scenario.singleTap(excessLossDb = 0.25, tapStartDay = 10)
    val sp = w.spans.head
    val envIdx = EnvIndex(w.envFor(sp.id))
    val ts = traces(w, sp, 1550, 30, seed = 99L)
    val evs = ChangePointScanner.scan(sp, ts, envIdx, ChangePointScanner.Config())
    assert(evs.exists(e => math.abs(e.edgeKm - 15.6) * 1000.0 < 100.0),
      s"got ${evs.map(e => (e.edgeKm, e.lossDb))}")
  }
}
