package mini
package cache

import chisel3._
import foam._

class Frontend(cache: Cache, fsmHandle: ChiselFSMHandle) {
  //get the next address and mask from the datapath
  cache.nextAddress := cache.cpu.req.bits.addr
  cache.cpu.resp.valid := false.B

  //after we've got a valid req, we can move onto the next req
  when(cache.cpu.resp.valid) {
    cache.address := cache.nextAddress
  }

  def read() = {
    when(fsmHandle("sIdle")) {
      cache.cpu.resp.valid := true.B
    }

    when(fsmHandle("sReadCache")) {
      when(cache.hit) {
        cache.cpu.resp.valid := true.B
      }
    }

    fsmHandle("readReq") := cache.cpu.req.valid && !cache.cpu.req.bits.mask.orR //leave idle when an incoming request happens
    fsmHandle("readFinish") := !cache.cpu.req.valid && cache.hit //leave read when we've got the right data, but the cpu is ready to move on
  }

  def write() = {
    fsmHandle("writeReq") := cache.cpu.req.valid && cache.cpu.req.bits.mask.orR
  }

}