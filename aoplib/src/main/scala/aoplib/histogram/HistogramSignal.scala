package aoplib.histogram

import chisel3._

/** Specifies signal whose values will be histogrammed after execution
  *
  * Can overwrite functions to customize the histogram behavior
  *
  * @param signal Signal to histogram
  */
class HistogramSignal(val signal: Bits) {
  def maxCount = 100
  def minValue: Int = signal match {
    case _: UInt => 0
    case s: SInt => Math.pow(2, s.getWidth - 1).toInt
  }
  /* Until max is the smallest illegal value, or the max legal value plus 1 */
  def untilMax: Int = signal match {
    case u: UInt => Math.pow(2, u.getWidth).toInt
    case s: SInt => Math.pow(2, s.getWidth - 1).toInt
  }
  def nBins: Int = untilMax - minValue
}
