package tapdetect

import tapdetect.engine.Pipeline
import tapdetect.report.{Evaluator, Renderer}
import tapdetect.sim.{DatasetBuilder, Simulator}

/** 命令行入口。
  *
  *   sbt "run demo"                 默认场景（8 条缆段 × 4 纤 × 30 天）
  *   sbt "run demo --scale 3"       3 倍规模（规模测试）
  *   sbt "run demo --days 60"
  *   sbt "run demo --seed 42"
  *   sbt "run eval --runs 10"        多随机种子的稳健性评估
  *   sbt "run eval --runs 10 --days 45"
  */
object Main {

  def main(args: Array[String]): Unit = {
    val cmd = if (args.isEmpty) "demo" else args(0)
    val opts = parseOpts(args.drop(1))
    cmd match {
      case "demo" => runDemo(opts)
      case "eval" => EvalMain.main(args.drop(1))
      case other =>
        System.err.println(s"unknown command: $other")
        sys.exit(2)
    }
  }

  private def parseOpts(args: Array[String]): Map[String, String] = {
    val m = scala.collection.mutable.Map.empty[String, String]
    var i = 0
    while (i < args.length) {
      if (args(i).startsWith("--")) {
        val k = args(i).drop(2)
        val v = if (i + 1 < args.length && !args(i + 1).startsWith("--")) args(i + 1) else "true"
        m(k) = v
        i += (if (v == "true") 1 else 2)
      } else i += 1
    }
    m.toMap
  }

  private def runDemo(opts: Map[String, String]): Unit = {
    val scale = opts.get("scale").map(_.toInt).getOrElse(1)
    val days = opts.get("days").map(_.toInt).getOrElse(30)
    val seed = opts.get("seed").map(_.toLong).getOrElse(20240117L)

    val simCfg = Simulator.Config(
      seed = seed,
      spanCount = 8 * scale,
      fibersPerCable = 4,
      days = days
    )
    val t0 = System.nanoTime()
    println(f"生成仿真数据：${simCfg.spanCount} 条缆段 × ${simCfg.fibersPerCable} 纤 × ${days} 天 × 3 波长 …")
    // 超过 8 条缆段（≈ 32 通道）时走流式路径：全量物化会占用数十 GB
    // （128 通道 × 30 天 × 3 波长 × 45 km × 0.5 m 采样 ≈ 17 亿个 double）。
    val streaming = simCfg.spanCount > 8
    val world = DatasetBuilder.buildWorldOnly(simCfg)
    val t1 = System.nanoTime()
    println(f"世界构建完成：${(t1 - t0) / 1e9}%.1f s，" +
      f"${world.taps.length} 个真值窃听事件，${world.spans.length} 条光纤通道" +
      (if (streaming) "（流式模式）" else ""))

    println("执行检测 …")
    val result =
      if (streaming) Pipeline.runStreaming(DatasetBuilder.stream(world, simCfg, seed), world.spans.length, Pipeline.Config())
      else Pipeline.run(DatasetBuilder.stream(world, simCfg, seed).toVector, Pipeline.Config())
    val t2 = System.nanoTime()
    println(f"检测完成（含数据生成）：${(t2 - t1) / 1e9}%.1f s\n")

    println(Renderer.renderDiagnostics(result.diagnostics))
    println()
    println(Renderer.renderAlarms(result.alarms))

    val truth = Evaluator.Truth(world.taps, world.spans.map(s => s.id -> s).toMap, world.start)
    val report = Evaluator.evaluate(result.alarms, truth)
    println(Evaluator.render(report))

    val out = new java.io.File("out")
    out.mkdirs()
    writeFile(new java.io.File(out, "alarms.csv"), Renderer.renderCsv(result.alarms))
    writeFile(new java.io.File(out, "alarms.txt"), Renderer.renderAlarms(result.alarms))
    writeFile(new java.io.File(out, "evaluation.txt"), Evaluator.render(report))
    writeFile(new java.io.File(out, "diagnostics.txt"), Renderer.renderDiagnostics(result.diagnostics))
    println("结果已写入 out/alarms.csv、out/alarms.txt、out/evaluation.txt")
  }

  private def writeFile(f: java.io.File, s: String): Unit = {
    val w = new java.io.PrintWriter(f, "UTF-8")
    try w.write(s) finally w.close()
  }
}
