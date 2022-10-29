package mini
package cache

import chisel3._
import foam._
import foam.aspects._

trait HasBufferBookeeping extends Cache {
  val v = bufferBookkeeping(fsmHandle)

  private def bufferBookkeeping(fsmHandle: ChiselFSMHandle) = {
    //simple bookkeeping
    val bufferTag = Reg(UInt(p.tlen.W))
    hit := tag === bufferTag && valids(index)

    // Take the cache line and split it into its words, get the word we want
    val cacheLine = VecInit.tabulate(p.nWords)(i => buffer.asUInt((i + 1) * xlen - 1, i * xlen))
    val reqData = cacheLine(offset)

    //update our simple bookkeeping
    when(mainMem.r.fire) {
      bufferTag := tag
    }

    //have to set this to true to keep the processor executing
    when(fsmHandle("sRefill")) {
      valids := valids.bitSet(index, true.B)
    }

    //give the CPU the data when it's ready
    cpu.resp.bits.data := reqData

    valids
  }
}