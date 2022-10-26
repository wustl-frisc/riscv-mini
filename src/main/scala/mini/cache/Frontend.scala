package mini
package cache

import chisel3._
import chisel3.util._
import foam._

class Frontend(p: CacheParams, io: CacheIO) {
  def read(hit: Bool)(implicit fsmHandle: ChiselFSMHandle) = {
    when(fsmHandle("sIdle")) {
      io.resp.valid := true.B
    }

    when(fsmHandle("sReadCache")) {
      when(hit) {
        io.resp.valid := true.B
      }
    }

    fsmHandle("readReq") := io.req.valid && !io.req.bits.mask.orR //leave idle when an incoming request happens
    //leave read when we've got the right data, but the cpu is ready to move on
    fsmHandle("readFinish") := !io.req.valid && hit
  }

  def write(offset: UInt, readDone: Bool = false.B)(implicit fsmHandle: ChiselFSMHandle) = {
    val fromCPUdata = Reg(chiselTypeOf(io.req.bits.data))
    val fromCPUmask = Reg(chiselTypeOf(io.req.bits.data))

    //clock in the next data and mask when we've resolved the last request
    when(io.resp.valid) {
      fromCPUdata := io.req.bits.data
      fromCPUmask := io.req.bits.mask
    }

    //construct what we need to send to the middle and back ends
    val writeData = Fill(p.nWords, fromCPUdata)
    val wordMask = VecInit.tabulate(p.nWords)(i => 0.U(p.wBytes.W))
    wordMask(offset) := fromCPUmask
    val writeMask =
      VecInit.tabulate(p.dataBeats)(i => Cat(wordMask((i + 1) * p.dataBeats - 1), wordMask(i * p.dataBeats)))

    //set up the conditions to start a write
    fsmHandle("writeReq") := io.req.valid && io.req.bits.mask.orR

    //bailout if we have an exception from the datapath
    fsmHandle("writeFinish") := io.abort || readDone

    (writeData, writeMask)
  }

}
