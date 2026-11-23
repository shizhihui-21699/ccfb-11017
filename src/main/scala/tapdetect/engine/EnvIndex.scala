package tapdetect.engine

import java.time.Instant
import tapdetect.model.EnvSample

/** 环境样本的时间索引：按时间二分查找 + 线性插值。
  *
  * 检测器不直接看到"环境噪声真值"，只能看到传感器读数 —— 这里提供的是把
  * 传感器读数对齐到任意测量时刻的能力。
  */
final class EnvIndex(val samples: Array[EnvSample]) {
  // 排序必须作用在样本本身，否则二分找到的下标与 times 对不上
  private val sorted: Array[EnvSample] = samples.sortBy(_.at.toEpochMilli)
  private val times: Array[Long] = sorted.map(_.at.toEpochMilli)

  def isEmpty: Boolean = samples.isEmpty

  /** 在某时刻插值出盒内温度与施工等级。 */
  def at(t: Instant): (Double, Double, Double) = {
    if (sorted.isEmpty) return (15.0, 15.0, 0.0)
    val key = t.toEpochMilli
    val idx = java.util.Arrays.binarySearch(times, key)
    if (idx >= 0) {
      val s = sorted(idx)
      (s.boxTempC, s.airTempC, s.constructionLevel)
    } else {
      val ins = -idx - 1
      if (ins == 0) {
        val s = sorted(0); (s.boxTempC, s.airTempC, s.constructionLevel)
      } else if (ins >= sorted.length) {
        val s = sorted.last; (s.boxTempC, s.airTempC, s.constructionLevel)
      } else {
        val a = sorted(ins - 1)
        val b = sorted(ins)
        val ta = a.at.toEpochMilli
        val tb = b.at.toEpochMilli
        val f = if (tb == ta) 0.0 else (key - ta).toDouble / (tb - ta)
        (a.boxTempC + f * (b.boxTempC - a.boxTempC),
          a.airTempC + f * (b.airTempC - a.airTempC),
          a.constructionLevel + f * (b.constructionLevel - a.constructionLevel))
      }
    }
  }

  def boxTemp(t: Instant): Double = at(t)._1
  def construction(t: Instant): Double = at(t)._3
}

object EnvIndex {
  def apply(samples: Vector[EnvSample]): EnvIndex = new EnvIndex(samples.toArray)
  val empty: EnvIndex = new EnvIndex(Array.empty)
}
