package mini
package aspects

import faust._
import chisel3._
import chisel3.util._
import mini.cache._

class AddCSR extends Aspect {
  Around(("CSR", "generateCSRMap"), "Seq[(BitPat,UInt)]") { (xlen: Int) => {
      val oldMap = proceed()
      val newReg = RegInit(0.U(xlen.W))
      oldMap.asInstanceOf[Seq[(BitPat,UInt)]] ++ Seq(
        BitPat(0x111.U(12.W)) -> newReg
      )
    }
  }
}