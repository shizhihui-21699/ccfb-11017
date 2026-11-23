package tapdetect.sim

import java.time.{Duration, Instant}
import java.util.Random
import tapdetect.engine.Pipeline
import tapdetect.model._

/** 生成仿真数据集（多段 × 多纤 × 多波长 × N 天）。
  *
  * 两种模式：
  *   - `build`       一次性物化全部曲线。适合小规模、需要反复复用数据的场景（测试、演示）；
  *   - `stream`      逐缆段生成并立刻交给消费者，内存只与"单条缆段 × 并行度"有关。
  *                   这是**唯一能处理上万条缆段**的模式：128 通道 × 30 天 × 3 波长 ×
  *                   平均 45 km × 0.5 m 采样 ≈ 17 亿个 double ≈ 13 GB。
  *
  * 数据组织模仿真实采集系统：
  *  - OTDR：每夜维护窗口测一次，三个波长各一条
  *  - 光功率：5 分钟一个点，三个波长各一路
  *  - 环境：5 分钟一个点（温度、施工）
  */
object DatasetBuilder {

  final case class Bundle(
      world: World,
      data: Vector[Pipeline.SpanData],
      cfg: Simulator.Config
  )

  /** 世界（台账 + 真值 + 环境）总是先整体生成：它只占 KB–MB 级，且评估需要真值。 */
  def buildWorldOnly(cfg: Simulator.Config): World = Simulator.buildWorld(cfg)

  /** 为一条缆段生成观测数据。`rng` 由调用方管理以控制确定性。 */
  def spanData(world: World, cfg: Simulator.Config, span: Span, rng: Random): Pipeline.SpanData = {
    val env = world.envFor(span.id)
    val tracesByWl: Map[Int, Vector[OtdrTrace]] = span.wavelengths.map { wl =>
      val traces = (0 until cfg.days).map { day =>
        val nightHour = 22 + rng.nextDouble() * 6.0
        val at = cfg.start.plus(Duration.ofDays(day.toLong))
          .plus(Duration.ofSeconds((nightHour * 3600).toLong))
        Simulator.otdrTrace(world, span, at, wl, rng,
          pulseWidthNs = if (wl == 1310) 30.0 else 20.0,
          sampleSpacingM = 0.5)
      }.toVector
      wl -> traces
    }.toMap
    val powerByWl: Map[Int, PowerSeries] = span.wavelengths.map { wl =>
      wl -> Simulator.powerSeries(world, span, wl, rng, cfg)
    }.toMap
    Pipeline.SpanData(span, tracesByWl, powerByWl, env)
  }

  def build(cfg: Simulator.Config, seed: Long = -1L): Bundle = {
    val world = Simulator.buildWorld(cfg)
    val rng = new Random(if (seed >= 0) seed else cfg.seed ^ 0x5DEECE66DL)
    val data = world.spans.map(span => spanData(world, cfg, span, rng))
    Bundle(world, data, cfg)
  }

  /** 流式生成：逐缆段产出 `SpanData`，调用方消费后即可释放。 */
  def stream(world: World, cfg: Simulator.Config, seed: Long = -1L): Iterator[Pipeline.SpanData] = {
    val rng = new Random(if (seed >= 0) seed else cfg.seed ^ 0x5DEECE66DL)
    world.spans.iterator.map(span => spanData(world, cfg, span, rng))
  }
}
