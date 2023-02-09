package mini.counter

import chisel3._

trait OnRisingEdge extends Counter {
  override def count(event: Bool) = {
    val prevState = RegNext(event)

    when(!prevState && event) {
      counterReg := counterReg + 1.U
    }
  }
}
