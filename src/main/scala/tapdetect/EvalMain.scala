package tapdetect

import tapdetect.engine.Pipeline
import tapdetect.report.Evaluator
import tapdetect.sim.{DatasetBuilder, Simulator}

/** 稳健性评估：多个随机种子 × 多个场景，统计检出率/误报/定位的分布。
  *
  * 单次 demo 的数字有随机性，不能作为"系统水平"的证据。本命令跑多次并给出
  * 中位数与区间，这是判断系统是否真的可用的唯一方式。
  *
  *   sbt "run eval"                 默认 5 个种子
  *   sbt "run eval --runs 20"
  *   sbt "run eval --days 45"
  */
object EvalMain {

  def main(args: Array[String]): Unit = {
    val opts = args.dropWhile(!_.startsWith("--")).grouped(2).collect {
      case Array(k, v) => k.drop(2) -> v
    }.toMap
    val runs = opts.get("runs").map(_.toInt).getOrElse(5)
    val days = opts.get("days").map(_.toInt).getOrElse(30)
    val scale = opts.get("scale").map(_.toInt).getOrElse(1)

    println(f"稳健性评估：$runs 个随机种子，每网 ${8 * scale} 缆段 × 4 纤 × $days 天\n")

    val rows = (0 until runs).map { r =>
      val seed = 1000L + r * 7919L
      val simCfg = Simulator.Config(seed = seed, spanCount = 8 * scale, fibersPerCable = 4, days = days)
      val t0 = System.nanoTime()
      val bundle = DatasetBuilder.build(simCfg, seed)
      val t1 = System.nanoTime()
      val result = Pipeline.run(bundle.data, Pipeline.Config())
      val t2 = System.nanoTime()
      val truth = Evaluator.Truth(bundle.world.taps,
        bundle.world.spans.map(s => s.id -> s).toMap, bundle.world.start)
      val report = Evaluator.evaluate(result.alarms, truth)
      println(f"种子 $seed%6d：真值 ${report.nTruth}%2d，告警 ${report.nAlarms}%2d，" +
        f"命中 ${report.matches.length}%2d，召回 ${report.recall}%.2f，" +
        f"准确 ${report.precision}%.2f，定位 ${report.medianKmErrorM}%.0f m，" +
        f"生成 ${(t1 - t0) / 1e9}%.1f s 检测 ${(t2 - t1) / 1e9}%.1f s" +
        (if (result.diagnostics.failedSpans > 0) s"，失败 ${result.diagnostics.failedSpans} 段" else ""))
      report
    }

    val recalls = rows.map(_.recall).filterNot(_.isNaN).toArray
    val precisions = rows.map(_.precision).filterNot(_.isNaN).toArray
    val kmErrs = rows.map(_.medianKmErrorM).filterNot(_.isNaN).toArray

    println("\n=== 汇总（跨种子）===")
    def stat(name: String, xs: Array[Double], unit: String = ""): Unit = {
      if (xs.isEmpty) println(f"$name%s：n/a")
      else println(f"$name%s：中位数 ${tapdetect.core.Stats.median(xs)}%.2f$unit%s  " +
        f"范围 [${xs.min}%.2f, ${xs.max}%.2f]$unit%s")
    }
    stat("检出率", recalls)
    stat("准确率", precisions)
    stat("定位误差中位数", kmErrs, " m")

    val totalTruth = rows.map(_.nTruth).sum
    val totalHits = rows.map(_.matches.length).sum
    val totalAlarms = rows.map(_.nAlarms).sum
    println(f"\n合计：真值 $totalTruth，命中 $totalHits（总体召回 ${totalHits.toDouble / math.max(1, totalTruth)}%.2f），" +
      f"告警 $totalAlarms（总体准确 ${totalHits.toDouble / math.max(1, totalAlarms)}%.2f）")
  }
}
