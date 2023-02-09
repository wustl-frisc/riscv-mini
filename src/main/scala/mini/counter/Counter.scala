package mini.counter

import chisel3._

abstract class Counter(width: Int = 64) {
  protected val counterReg = RegInit(0.U(width.W))

  def count(event: Bool): Unit
  def getCount = counterReg
  def toPrintable(name: String) = cf"$name: $counterReg\n"
}
