package tapdetect.engine

import tapdetect.core.{Inference, Stats}
import tapdetect.model._

/** 把波长的原始曲线转成"候选事件"，并把三波长的证据按里程对齐。
  *
  * 这是 ChangePointScanner 的编排层：
  *   - 对每个波长单独扫描；
  *   - 把三个波长的候选按里程（±容差）与时间（±容差）配对，得到每处事件的
  *     "三波长插损谱"，交给 WavelengthAnalyzer 判断是否呈宏弯特征。
  */
object OtdrAnalyzer {

  final case class Config(
      scanner: ChangePointScanner.Config = ChangePointScanner.Config(),
      /** 跨波长配对时允许的里程差（m） */
      crossWavelengthKmTolM: Double = 200.0,
      /** 跨波长配对时允许的起始时刻差（小时） */
      crossWavelengthHours: Double = 96.0
  )

  /** 一处"三波长已配对"的事件。 */
  final case class Event(
      spanId: String,
      antennaKm: Double,              // 主波长给出的位置
      lossByWavelength: Map[Int, Double],
      zByWavelength: Map[Int, Double],
      persistence: Double,
      onsetTime: java.time.Instant,
      distToSpliceM: Double,
      primaryWavelengthNm: Int,
      perWavelength: Map[Int, ChangePointScanner.SpatialEvent]
  ) {
    def primaryLossDb: Double = lossByWavelength.getOrElse(primaryWavelengthNm,
      lossByWavelength.values.maxOption.getOrElse(0.0))
    def maxZ: Double = if (zByWavelength.isEmpty) 0.0 else zByWavelength.values.max
  }

  /** 对一条光缆段的所有波长做扫描 + 配对。 */
  def analyze(
      span: Span,
      tracesByWavelength: Map[Int, Vector[OtdrTrace]],
      env: EnvIndex,
      cfg: Config
  ): Vector[Event] = {
    val perWl: Map[Int, Vector[ChangePointScanner.SpatialEvent]] =
      tracesByWavelength.map { case (wl, traces) =>
        wl -> ChangePointScanner.scan(span, traces, env, cfg.scanner)
      }

    val primary = if (perWl.contains(span.primaryWavelengthNm)) span.primaryWavelengthNm
                  else perWl.keys.maxOption.getOrElse(span.primaryWavelengthNm)

    // 以主波长的候选为骨架，把其他波长上"同位置、同时段"的事件挂上去
    val base = perWl.getOrElse(primary, Vector.empty)
    val others = perWl.filter(_._1 != primary)

    base.map { b =>
      val los = scala.collection.mutable.Map(primary -> b.lossDb)
      val zs = scala.collection.mutable.Map(primary -> b.z)
      val matched = scala.collection.mutable.Map(primary -> b)
      others.foreach { case (wl, evs) =>
        evs.filter { e =>
          math.abs(e.edgeKm - b.edgeKm) * 1000.0 <= cfg.crossWavelengthKmTolM &&
            math.abs(java.time.Duration.between(e.onsetTime, b.onsetTime).toHours) <=
              cfg.crossWavelengthHours
        }.sortBy(e => math.abs(e.edgeKm - b.edgeKm)).headOption.foreach { e =>
          los(wl) = e.lossDb
          zs(wl) = e.z
          matched(wl) = e
        }
      }
      Event(
        spanId = span.id,
        antennaKm = b.edgeKm,
        lossByWavelength = los.toMap,
        zByWavelength = zs.toMap,
        persistence = b.persistence,
        onsetTime = b.onsetTime,
        distToSpliceM = b.distToSpliceM,
        primaryWavelengthNm = primary,
        perWavelength = matched.toMap
      )
    }
  }
}
