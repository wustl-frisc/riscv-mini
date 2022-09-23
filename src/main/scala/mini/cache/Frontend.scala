package mini
package cache

import chisel3._
import foam._

class Frontend(fsmHandle: ChiselFSMHandle, io: CacheIO, hit: Bool) {
  def read() = {
    when(fsmHandle("sIdle")) {
      io.resp.valid := true.B
    }

    when(fsmHandle("sReadCache")) {
      when(hit) {
        io.resp.valid := true.B
      }
    }

    fsmHandle("readReq") := io.req.valid && !io.req.bits.mask.orR //leave idle when an incoming request happens
    fsmHandle("readFinish") := !io.req.valid && hit //leave read when we've got the right data, but the cpu is ready to move on
  }

  def write() = {
    val data = Reg(chiselTypeOf(io.req.bits.data))
    val mask = Reg(chiselTypeOf(io.req.bits.data))

    //clock in the next data and mask when we've resolved the last request
    when(io.resp.valid) {
      data := io.req.bits.data
      mask := io.req.bits.mask
    }

    fsmHandle("writeReq") := io.req.valid && io.req.bits.mask.orR

    (data, mask)
  }

}