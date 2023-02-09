package mini.counter

import chisel3._

trait OnHigh extends Counter{
  override def count(event: Bool) = {
    when(event) {
      counterReg := counterReg + 1.U
    }
  }
}
