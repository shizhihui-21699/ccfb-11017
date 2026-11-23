package tapdetect.model

import java.time.Instant

/** 一条 OTDR 测量曲线。
  *
  * @param spanId    所属光缆段
  * @param wavelengthNm 测量波长
  * @param measuredAt 测量时刻
  * @param startKm   曲线第一个采样点对应的里程
  * @param spacingM  相邻采样点间距（m）。OTDR 采样间隔由脉冲宽度和采样率共同决定，
  *                  窄脉冲分辨率高但动态范围小 —— 窃听点定位必须用窄脉冲。
  * @param valuesDb  各采样点的背向散射功率（dB，已扣除发射功率和前端盲区，相对值）
  * @param traceId   数据源中的曲线 ID（便于回溯原始曲线给现场做比对）
  */
final case class OtdrTrace(
    traceId: String,
    spanId: String,
    wavelengthNm: Int,
    measuredAt: Instant,
    startKm: Double,
    spacingM: Double,
    valuesDb: Array[Double]
) {
  def length: Int = valuesDb.length

  @inline def kmAt(i: Int): Double = startKm + i * spacingM / 1000.0

  @inline def indexAt(km: Double): Int = math.round((km - startKm) * 1000.0 / spacingM).toInt

  /** 口径：每米衰减的中位数（dB/m），用于粗校准。 */
  def medianSlopeDbPerM: Double = {
    val diffs = new Array[Double](length - 1)
    var i = 0
    while (i < length - 1) { diffs(i) = valuesDb(i + 1) - valuesDb(i); i += 1 }
    val sorted = diffs.sorted
    sorted(sorted.length / 2)
  }
}

/** 单波长光功率监测序列（在线 OPM / 光功率计，分钟级采样）。 */
final case class PowerSeries(
    spanId: String,
    wavelengthNm: Int,
    start: Instant,
    stepSeconds: Long,
    valuesDbm: Array[Double]
) {
  def length: Int = valuesDbm.length
  @inline def timeAt(i: Int): Instant = start.plusSeconds(stepSeconds * i)

  /** 滑动差分：返回 (index, deltaDbm) 步骤变化序列。 */
  def diffs: Array[(Int, Double)] =
    (1 until length).map(i => (i, valuesDbm(i) - valuesDbm(i - 1))).toArray
}

/** 环境上下文：温度和施工事件。真实系统中来自环境温度传感器 / 施工工单 / 光纤振动传感。 */
final case class EnvSample(
    spanId: String,
    at: Instant,
    /** 接头盒内温度（°C），可由接头盒内温度传感器或气象站+日照模型给出 */
    boxTempC: Double,
    /** 光缆入地/架空段的附近气温（°C） */
    airTempC: Double,
    /** 施工干扰标志：0 正常，>0 表示有施工事件强度（0..1） */
    constructionLevel: Double = 0.0,
    /** 是否处于计划性割接/维护窗口 */
    maintenanceWindow: Boolean = false
)
