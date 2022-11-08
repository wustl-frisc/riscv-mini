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

  def read(
    address: UInt,
    buffer:  Vec[UInt],
    hit:     Bool,
    dirty:   Bool,
    isRead:  Bool
  ) = {
    require(p.dataBeats > 0)
    val (read_count, read_wrap_out) = Counter(io.r.fire, p.dataBeats)

    //if we're here, the current address was a miss, ask for the data from the backing store
    io.ar.bits := NastiAddressBundle(p.nasti)(
      0.U,
      address,
      log2Up(p.nasti.dataBits / 8).U,
      (p.dataBeats - 1).U
    )

    when(io.r.fire) {
      buffer(read_count) := io.r.bits.data
    }

    when(fsmHandle("sReadCache")) {
      //when the data is stale, we go get some new fresh data
      when(!hit && !dirty) {
        io.ar.valid := true.B
      }
    }

    when(fsmHandle("sRefill")) {
      //let the main memory we're ready
      io.r.ready := true.B
    }

    fsmHandle("readMiss") := io.ar.fire //leave the read state when mem has recieved our address
    fsmHandle("refillFinish") := read_wrap_out && isRead // leave the refill state when we've got our data

    //retrun the signal to say that the read is done
    read_wrap_out
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

  def write(
    address:    UInt,
    data:       UInt,
    mask:       Option[Vec[UInt]],
    offset:     UInt,
    localWrite: Bool,
    dirty:      Bool
  ) = {
    require(p.dataBeats > 0)
    val (write_count, write_wrap_out) = Counter(io.w.fire, p.dataBeats)

    //set up write address channel
    io.aw.bits := NastiAddressBundle(p.nasti)(
      0.U,
      address,
      log2Up(p.nasti.dataBits / 8).U,
      (p.dataBeats - 1).U
    )

    //setup write channel -- this is a full cacheline with our mask
    io.w.bits := NastiWriteDataBundle(p.nasti)(
      VecInit.tabulate(p.dataBeats)(i => data((i + 1) * p.nasti.dataBits - 1, i * p.nasti.dataBits))(write_count),
      mask match {
        case None          => None
        case Some(bitMask) => Some(bitMask(write_count))
      },
      write_wrap_out
    )

    //tell the memory to get ready to write to the address
    when(fsmHandle("sWriteCache")) {
      when(!localWrite && dirty) {
        io.aw.valid := true.B
      }
    }

    //memory ready for write
    fsmHandle("writeMiss") := io.aw.fire

    //tell the memory our data is valid
    when(fsmHandle("sWriteSetup")) {
      io.w.valid := true.B
    }

    //when we're done writing, wait for ack
    fsmHandle("memWait") := write_wrap_out

    //say we're ready for ack
    when(fsmHandle("sWriteWait")) {
      io.b.ready := true.B
    }

    //write has been acknowledged
    fsmHandle("ack") := io.b.fire
  }
}
