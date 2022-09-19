package mini
package cache

import chisel3._
import chisel3.util._
import junctions._
import foam._

class Backend(cache: Cache, fsmHandle: ChiselFSMHandle) {
  //read address
  cache.mainMem.ar.valid := false.B
  //read data
  cache.mainMem.r.ready := false.B
  //write address
  cache.mainMem.aw.valid := false.B
  //write data
  cache.mainMem.w.valid := false.B
  //write response
  cache.mainMem.b.ready := false.B

  def read(buffer: Vec[UInt], bufferTag: UInt) = {
    require(cache.p.dataBeats > 0)
    val (read_count, read_wrap_out) = Counter(cache.mainMem.r.fire, cache.p.dataBeats)

    //if we're here, the current address was a miss, ask for the data from the backing store
    cache.mainMem.ar.bits := NastiAddressBundle(cache.nasti)(
      0.U,
      (cache.address(cache.xlen - 1, cache.p.offsetLen) << cache.p.offsetLen.U).asUInt,
      log2Up(cache.nasti.dataBits / 8).U,
      (cache.p.dataBeats - 1).U
    )

    when(cache.mainMem.r.fire) {
      buffer(read_count) := cache.mainMem.r.bits.data
      bufferTag := cache.tag
    }

    when(fsmHandle("sReadCache")) {
      //when the data is stale, we go get some new fresh data
      when(!cache.hit) {
        cache.mainMem.ar.valid := true.B
      }
    }

    when(fsmHandle("sRefill")) {
      //let the main memory we're ready
      cache.mainMem.r.ready := true.B
    }

    fsmHandle("cleanMiss") := cache.mainMem.ar.fire //leave the read state when mem has recieved our address
    fsmHandle("refillFinish") := read_wrap_out // leave the refill state when we've got our data

    //retrun the signal to say that the read is done
    read_wrap_out
  }

  def sparceWrite(data: UInt, mask: UInt) = {
    require(cache.p.dataBeats > 0)
    val (write_count, write_wrap_out) = Counter(cache.mainMem.w.fire, cache.p.dataBeats)

    cache.mainMem.aw.bits := NastiAddressBundle(cache.nasti)(
      0.U,
      (cache.address(cache.xlen - 1, cache.p.offsetLen) << cache.p.offsetLen.U).asUInt,
      log2Up(cache.nasti.dataBits / 8).U,
      (cache.p.dataBeats - 1).U
    )

    cache.mainMem.w.bits := NastiWriteDataBundle(cache.nasti)(
      0.U,
      None,
      write_wrap_out
    )

    write_wrap_out
  }

  def writeStub() = {
    // write addr
    cache.mainMem.aw.bits := NastiAddressBundle(cache.nasti)(
      0.U,
      0.U,
      0.U,
      0.U
    )
    // write data
    cache.mainMem.w.bits := NastiWriteDataBundle(cache.nasti)(
      0.U,
      None,
      false.B
    )
  }
}