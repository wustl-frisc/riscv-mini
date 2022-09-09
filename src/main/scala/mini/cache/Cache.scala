// See LICENSE for license details.

package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import junctions._
import foam._

class CacheReq(addrWidth: Int, dataWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
  val mask = UInt((dataWidth / 8).W)
}

class CacheResp(dataWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
}

class CacheIO(addrWidth: Int, dataWidth: Int) extends Bundle {
  val abort = Input(Bool())
  val req = Flipped(Valid(new CacheReq(addrWidth, dataWidth)))
  val resp = Valid(new CacheResp(dataWidth))
}

case class CacheConfig(nWays: Int, nSets: Int, blockBytes: Int)

class MetaData(tagLength: Int) extends Bundle {
  val tag = UInt(tagLength.W)
}

class CacheParams(val nSets: Int, blockBytes: Int, xlen: Int, dataBits: Int) {
  println("Cache Config:")

  val bBytes = blockBytes
  println("\tblock bytes: " + bBytes)

  val bBits = bBytes << 3
  println("\tblock bits: " + bBits)

  val offsetLen = log2Ceil(bBytes)
  println("\toffset len: " + offsetLen)

  val indexLen = log2Ceil(nSets)
  println("\tindex len: " + indexLen)

  val tlen = xlen - (indexLen + offsetLen)
  println("\ttag len: " + tlen)

  val nWords = bBits / xlen
  println("\twords: " + nWords)

  val wBytes = xlen / 8
  println("\tword bytes: "+ wBytes)

  val byteOffsetBits = log2Ceil(wBytes)
  println("\tbyte offset bits: " + byteOffsetBits)

  val dataBeats = bBits / dataBits
  println("\tdata beats: " + dataBeats)
}


class Cache(val c: CacheConfig, val nasti: NastiBundleParameters, val xlen: Int) extends Module {
  // local parameters
  val p = new CacheParams(c.nSets, c.blockBytes, xlen, nasti.dataBits)
  val cpu = IO(new CacheIO(xlen, xlen))
  val mainMem = IO(new NastiBundle(nasti))

  val sIdle = new CacheState("sIdle")
  val sReadCache = new CacheState("sReadCache")
  val sRefill = new CacheState("sRefill")

  val readReq = CacheToken("readReq")
  val readHit = CacheToken("readHit")
  val readFinish = CacheToken("readFinish")
  val cleanMiss = CacheToken("cleanMiss")
  val refillFinish = CacheToken("refillFinish")

  val cacheNFA = (new NFA(sIdle))
    .addTransition((sIdle, readReq), sReadCache)
    .addTransition((sReadCache, readHit), sReadCache)
    .addTransition((sReadCache, readFinish), sIdle)
    .addTransition((sReadCache, cleanMiss), sRefill)
    .addTransition((sRefill, refillFinish), sIdle)

  val fsmHandle = ChiselFSMBuilder(cacheNFA)

  //generate frontend
  frontend()

  //generate backend
  backend()

  private def frontend() = {

    val addr = cpu.req.bits.addr

    // Read Mux
    cpu.resp.bits.data := 0.U
    cpu.resp.valid := false.B

    // Cache FSM
    when(fsmHandle("sIdle")) {
      cpu.resp.valid := false.B
    }

    when(fsmHandle("sReadCache")) {
      cpu.resp.valid := true.B
    }

    fsmHandle("readReq") := cpu.req.valid
    fsmHandle("readHit") := cpu.req.valid
    fsmHandle("readFinish") := !cpu.req.valid
  }

  private def backend() = {

    mainMem.ar.bits := NastiAddressBundle(nasti)(
    0.U,
    0.U,
    0.U,
    0.U,
    )

    mainMem.ar.valid := false.B
    // read data
    mainMem.r.ready := false.B
    // write addr
    mainMem.aw.bits := NastiAddressBundle(nasti)(
      0.U,
      0.U,
      0.U,
      0.U
    )
    mainMem.aw.valid := false.B
    // write data
    mainMem.w.bits := NastiWriteDataBundle(nasti)(
      0.U,
      None,
      false.B
    )
    mainMem.w.valid := false.B
    // write resp
    mainMem.b.ready := false.B
  }
}
