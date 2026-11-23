package tapdetect.engine

import java.time.Instant
import tapdetect.core.Inference
import tapdetect.model._

/** 最终告警：面向调度/巡线人员的产物。 */
final case class Alarm(
    id: String,
    cableId: String,
    spanId: String,
    fiberNo: Int,
    /** 最可能的窃听位置（km，相对本段起点） */
    km: Double,
    /** 现场搜索窗口半宽（m） */
    windowM: Double,
    /** 估计的插入损耗（dB） */
    lossDb: Double,
    /** 首次出现时间（最近一次 OTDR 测量时刻） */
    onset: Instant,
    /** 置信度 0..1 */
    confidence: Double,
    severity: String,
    evidence: Vector[String],
    action: String,
    /** 现场位置描述（盘号 + 距接头盒） */
    locationHint: String,
    /** 三波长判定：bend-like / wavelength-flat / inconclusive */
    wavelengthVerdict: String,
    multiFiber: Boolean,
    /** 触发时使用的统计量，便于复核与调参 */
    stats: Map[String, Double]
)

/** 证据融合：把 OTDR 定位、三波长形状、多纤关联、光功率旁证合成一条告警。
  *
  * 置信度按**对数 Bayes 因子**累加，再经 sigmoid 变成概率。每条证据给出一个
  * 独立的似然比，这样不同缆段、不同波长组合下的阈值含义一致，也便于事后审计：
  * 任何一条告警都能回答"它是被哪几条证据推上去的"。
  */
object Fusion {

  final case class Config(
      /** 一次分析（整张网）的族错误率 */
      familyAlpha: Double = 0.01,
      lrBendLike: Double = 12.0,
      lrFlatLike: Double = 0.15,
      lrMultiFiber: Double = 8.0,
      lrPowerCorroborate: Double = 4.0,
      lrNearClosure: Double = 2.0,
      /** 远离熔接点 → 更不可能是温漂，LR 略提升 */
      lrFarFromSplice: Double = 1.6,
      /** 先验赔率：任一段光纤在任一天出现窃听的基础几率 */
      priorOdds: Double = 1.0 / 3000.0,
      minConfidence: Double = 0.5
  )

  final case class Candidate(
      span: Span,
      event: OtdrAnalyzer.Event,
      power: Option[PowerAnalyzer.PowerVerdict],
      cluster: Option[MultiFiberCorrelator.CableCluster],
      /** 该缆段被扫描时登记的"候选事件总数"，用于多重比较校正 */
      testsInSpan: Int
  )

  def fuse(candidates: Vector[Candidate], cfg: Config, alarmPrefix: String = "A"): Vector[Alarm] = {
    if (candidates.isEmpty) return Vector.empty

    val m = math.max(1, candidates.length)
    val alphaAdj = Inference.sidakAlpha(cfg.familyAlpha, m)
    val zGate = Inference.zCritical(alphaAdj)

    val scored = candidates.map { c =>
      val wl = WavelengthAnalyzer.classify(c.event.lossByWavelength)
      val isBend = wl.verdict == "bend-like"
      val isFlat = wl.verdict == "wavelength-flat"

      var logOdds = math.log(cfg.priorOdds)
      val ev = scala.collection.mutable.ArrayBuffer.empty[String]

      val z = c.event.maxZ
      val pNull = math.max(1e-12, Inference.twoSidedP(z))
      val lrSig = math.min(1e6, math.max(1.0, 0.8 / pNull))
      logOdds += math.log(lrSig)
      ev += f"OTDR 在该里程出现 ${c.event.primaryLossDb}%.2f dB 持续台阶（z=${z}%.1f，" +
        f"持续性 ${c.event.persistence}%.2f）"

      if (isBend) {
        logOdds += math.log(cfg.lrBendLike)
        ev += f"三波长损耗比 ${wl.longShortRatio}%.1f:1，符合宏弯特征" +
          f"（拟合弯曲半径 ${wl.fittedRadiusMm}%.1f mm）"
      } else if (isFlat) {
        logOdds += math.log(cfg.lrFlatLike)
        ev += "三波长损耗接近相等，更像温漂或熔接劣化"
      } else {
        val got = c.event.lossByWavelength.keys.toVector.sorted.mkString("/")
        ev += s"三波长证据不明确（仅在 $got nm 上看到台阶）"
      }

      c.cluster.foreach { cl =>
        if (cl.hits.length >= 2) {
          logOdds += math.log(cfg.lrMultiFiber)
          ev += f"同缆 ${cl.hits.length}/${cl.fiberCount} 根纤芯在同一里程、相近时段出现异常" +
            (if (cl.verdict == "closure-intrusion") "（提示接头盒被打开）" else "")
        }
      }

      c.power.foreach { pw =>
        logOdds += math.log(cfg.lrPowerCorroborate)
        ev += f"光功率监测同期出现 ${pw.lossDb}%.2f dB 持续下降（z=${pw.zScore}%.1f）"
      }

      val nearClosure = c.span.closureNear(c.event.antennaKm, 400.0).isDefined
      if (nearClosure) { logOdds += math.log(cfg.lrNearClosure); ev += "位置在接头盒 400m 内（窃听高发点）" }
      if (c.event.distToSpliceM > 300.0 && !c.event.distToSpliceM.isInfinity) {
        logOdds += math.log(cfg.lrFarFromSplice)
        ev += f"距最近熔接点 ${c.event.distToSpliceM}%.0f m，温度漂移解释力弱"
      }

      val confidence = 1.0 / (1.0 + math.exp(-logOdds))
      (c, wl, confidence, ev.toVector, z >= zGate)
    }

    scored.filter { case (_, _, conf, _, passZ) => passZ && conf >= cfg.minConfidence }
      .sortBy(-_._3)
      .zipWithIndex
      .map { case ((c, wl, conf, ev, _), idx) =>
        val win = math.min(120.0, math.max(20.0, 40.0 / math.sqrt(math.max(1.0, c.event.maxZ / 6.0))))
        val sev = if (conf >= 0.93) "高" else if (conf >= 0.8) "中" else "低"
        val action =
          if (sev == "高") "立即派巡线组现场核查；用 1550/1625 双波长从两端对测复核，并调阅该段光功率日曲线"
          else if (sev == "中") "24 小时内派单核查；先人工复核两端 OTDR 曲线与光功率历史"
          else "纳入观察清单，连续 3 天复测；台阶持续或增大再升级派单"
        Alarm(
          id = f"$alarmPrefix-${c.span.cableId}-${c.span.fiberNo}-${idx + 1}%02d",
          cableId = c.span.cableId,
          spanId = c.span.id,
          fiberNo = c.span.fiberNo,
          km = c.event.antennaKm,
          windowM = win,
          lossDb = c.event.primaryLossDb,
          onset = c.event.onsetTime,
          confidence = conf,
          severity = sev,
          evidence = ev,
          action = action,
          locationHint = c.span.describe(c.event.antennaKm, win),
          wavelengthVerdict = wl.verdict,
          multiFiber = c.cluster.exists(_.hits.length >= 2),
          stats = Map(
            "z" -> c.event.maxZ,
            "zGate" -> zGate,
            "persistence" -> c.event.persistence,
            "bendR2" -> wl.bendModelR2,
            "longShortRatio" -> wl.longShortRatio,
            "distToSpliceM" -> (if (c.event.distToSpliceM.isInfinity) -1.0 else c.event.distToSpliceM),
            "testCount" -> m.toDouble
          )
        )
      }

  }
}
