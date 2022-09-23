package mini
package cache

import chisel3._
import chisel3.util._
import junctions._
import foam._

class Backend(fsmHandle: ChiselFSMHandle, p: CacheParams, io: NastiBundle, address: UInt) {
  //read address
  io.ar.valid := false.B
  //read data
  io.r.ready := false.B
  //write address
  io.aw.valid := false.B
  //write data
  io.w.valid := false.B
  //write response
  io.b.ready := false.B

  def read(buffer: Vec[UInt], bufferTag: UInt, tag: UInt, hit: Bool) = {
    require(p.dataBeats > 0)
    val (read_count, read_wrap_out) = Counter(io.r.fire, p.dataBeats)

    //if we're here, the current address was a miss, ask for the data from the backing store
    io.ar.bits := NastiAddressBundle(p.nasti)(
      0.U,
      (address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt,
      log2Up(p.nasti.dataBits / 8).U,
      (p.dataBeats - 1).U
    )

    when(io.r.fire) {
      buffer(read_count) := io.r.bits.data
      bufferTag := tag
    }

    when(fsmHandle("sReadCache")) {
      //when the data is stale, we go get some new fresh data
      when(!hit) {
        io.ar.valid := true.B
      }
    }

    when(fsmHandle("sRefill")) {
      //let the main memory we're ready
      io.r.ready := true.B
    }

    fsmHandle("cleanMiss") := io.ar.fire //leave the read state when mem has recieved our address
    fsmHandle("refillFinish") := read_wrap_out // leave the refill state when we've got our data

    //retrun the signal to say that the read is done
    read_wrap_out
  }

  def sparceWrite(data: UInt, mask: UInt, valids: UInt, offset: UInt) = {
    require(p.dataBeats > 0)
    val (write_count, write_wrap_out) = Counter(io.w.fire, p.dataBeats)

    io.aw.bits := NastiAddressBundle(p.nasti)(
      0.U,
      (address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt,
      log2Up(p.nasti.dataBits / 8).U,
      (p.dataBeats - 1).U
    )

    val writeData = Fill(p.nWords, data)
    val wordMask = VecInit.tabulate(p.nWords)(i => 0.U(p.wBytes.W))
    wordMask(offset) := mask
    val writeMask = VecInit.tabulate(p.dataBeats)(i => Cat(wordMask((i + 1) * p.dataBeats - 1), wordMask(i * p.dataBeats)))

    io.w.bits := NastiWriteDataBundle(p.nasti)(
      VecInit.tabulate(p.dataBeats)(i => writeData((i + 1) * p.nasti.dataBits - 1, i * p.nasti.dataBits))(write_count),
      Some(writeMask(write_count)),
      write_wrap_out
    )

    when(fsmHandle("sWriteCache")) {
      io.aw.valid := true.B
    }

    fsmHandle("writeMiss") := io.aw.fire

    when(fsmHandle("sWriteSetup")) {
      io.w.valid := true.B
    }

    fsmHandle("memWait") := write_wrap_out

    val index = address(p.indexLen + p.offsetLen - 1, p.offsetLen)
    when(fsmHandle("sWriteWait")) {
      valids := valids.bitSet(index, false.B)
      io.b.ready := true.B
    }

    fsmHandle("ack") := io.b.fire

    io.b.fire
  }

  def writeStub() = {
    // write addr
    io.aw.bits := NastiAddressBundle(p.nasti)(
      0.U,
      0.U,
      0.U,
      0.U
    )
    // write data
    io.w.bits := NastiWriteDataBundle(p.nasti)(
      0.U,
      None,
      false.B
    )
  }
}