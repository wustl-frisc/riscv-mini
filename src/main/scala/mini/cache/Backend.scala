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

    val writeData = Fill(cache.p.nWords, data)
    val wordMask = VecInit.tabulate(cache.p.nWords)(i => 0.U(cache.p.wBytes.W))
    wordMask(cache.offset) := mask
    val writeMask = VecInit.tabulate(cache.p.dataBeats)(i => Cat(wordMask((i + 1) * cache.p.dataBeats - 1), wordMask(i * cache.p.dataBeats)))

    cache.mainMem.w.bits := NastiWriteDataBundle(cache.nasti)(
      VecInit.tabulate(cache.p.dataBeats)(i => writeData((i + 1) * cache.nasti.dataBits - 1, i * cache.nasti.dataBits))(write_count),
      Some(writeMask(write_count)),
      write_wrap_out
    )

    when(fsmHandle("sWriteCache")) {
      cache.mainMem.aw.valid := true.B
    }

    fsmHandle("writeMiss") := cache.mainMem.aw.fire

    when(fsmHandle("sWriteSetup")) {
      cache.mainMem.w.valid := true.B
    }

    fsmHandle("memWait") := write_wrap_out

    val index = cache.address(cache.p.indexLen + cache.p.offsetLen - 1, cache.p.offsetLen)
    when(fsmHandle("sWriteWait")) {
      cache.valids := cache.valids.bitSet(index, false.B)
      cache.mainMem.b.ready := true.B
    }

    fsmHandle("ack") := cache.mainMem.b.fire

    cache.mainMem.b.fire
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