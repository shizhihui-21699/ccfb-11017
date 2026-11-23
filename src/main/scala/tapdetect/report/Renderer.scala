package tapdetect.report

import tapdetect.engine.Alarm

/** 告警的文本输出（可读报告）与 CSV 输出（对接工单系统）。 */
object Renderer {

  def renderAlarms(alarms: Vector[Alarm]): String = {
    if (alarms.isEmpty) return "（无告警）\n"
    val sb = new StringBuilder
    alarms.foreach { a =>
      sb.append("─" * 78).append('\n')
      sb.append(f"【${a.severity}】${a.id}  ${a.cableId}  ${a.spanId}（第${a.fiberNo}纤）\n")
      sb.append(f"  位置：  ${a.km}%.3f km  ±${a.windowM}%.0f m\n")
      sb.append(f"  现场：  ${a.locationHint}\n")
      sb.append(f"  插损：  ${a.lossDb}%.3f dB    首次出现：${a.onset}\n")
      sb.append(f"  置信度：${a.confidence}%.2f       三波长判定：${a.wavelengthVerdict}" +
        (if (a.multiFiber) "       多纤关联：是" else "") + "\n")
      sb.append("  证据：\n")
      a.evidence.foreach(e => sb.append(s"    · $e\n"))
      sb.append(s"  建议：${a.action}\n")
      sb.append(f"  统计量：z=${a.stats("z")}%.1f  zGate=${a.stats("zGate")}%.1f  " +
        f"持续性=${a.stats("persistence")}%.2f  " +
        f"长/短波损耗比=${a.stats("longShortRatio")}%.1f  " +
        f"检验次数=${a.stats("testCount")}%.0f\n")
    }
    sb.append("─" * 78).append('\n')
    sb.toString
  }

  def renderCsv(alarms: Vector[Alarm]): String = {
    val header = "alarm_id,cable_id,span_id,fiber,km,window_m,loss_db,onset,confidence," +
      "severity,wavelength_verdict,multi_fiber,location_hint\n"
    def esc(s: String): String = "\"" + s.replace("\"", "\"\"") + "\""
    header + alarms.map { a =>
      s"${a.id},${a.cableId},${a.spanId},${a.fiberNo},${f"${a.km}%.3f"},${f"${a.windowM}%.0f"}," +
        s"${f"${a.lossDb}%.3f"},${a.onset},${f"${a.confidence}%.3f"},${a.severity}," +
        s"${a.wavelengthVerdict},${a.multiFiber},${esc(a.locationHint)}"
    }.mkString("\n") + "\n"
  }

  def renderDiagnostics(d: tapdetect.engine.Pipeline.Diagnostics): String = {
    val sb = new StringBuilder
    sb.append("=== 运行诊断 ===\n")
    sb.append(f"分析光缆段数：      ${d.spansAnalyzed}\n")
    sb.append(f"候选点数：          ${d.candidates}\n")
    sb.append(f"显著性门限淘汰：    ${d.suppressedByZ}\n")
    sb.append(f"置信度门限淘汰：    ${d.suppressedByConfidence}\n")
    sb.toString
  }
}
