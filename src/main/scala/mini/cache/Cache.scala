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
  val readFinish = CacheToken("readFinish")
  val cleanMiss = CacheToken("cleanMiss")
  val refillFinish = CacheToken("refillFinish")

  val cacheNFA = (new NFA(sIdle))
    .addTransition((sIdle, readReq), sReadCache)
    .addTransition((sReadCache, cleanMiss), sRefill)
    .addTransition((sReadCache, readFinish), sIdle)
    .addTransition((sRefill, refillFinish), sIdle)

  val fsmHandle = ChiselFSMBuilder(cacheNFA)


  val addr = Reg(UInt(xlen.W))
  val idx = addr(p.indexLen + p.offsetLen - 1, p.offsetLen)
  val tag = addr(xlen - 1, p.indexLen + p.offsetLen)
  val offset = addr(p.offsetLen - 1, p.byteOffsetBits)

  val buffer = Reg(Vec(p.dataBeats, UInt(nasti.dataBits.W)))
  val bufferTag = Reg(UInt(p.tlen.W))
  val hit = tag === bufferTag

  //generate frontend
  frontend()

  //generate backend
  backend()

  private def frontend() = {

    addr := cpu.req.bits.addr

    // Resp setup
    val cacheLine = VecInit.tabulate(p.nWords)(i => buffer.asUInt((i + 1) * xlen - 1, i * xlen))
    val reqData = cacheLine(offset)
    cpu.resp.bits.data := reqData
    cpu.resp.valid := false.B

    // Cache FSM
    when(fsmHandle("sIdle")) {
      printf("sIdle\n")
    }

    when(fsmHandle("sReadCache")) {
      when(hit) {
        cpu.resp.valid := true.B
      }
    }

    fsmHandle("readReq") := cpu.req.valid
    fsmHandle("readFinish") := !cpu.req.valid
    fsmHandle("cleanMiss") := cpu.req.valid
  }

  private def backend() = {

    require(p.dataBeats > 0)
    val (read_count, read_wrap_out) = Counter(mainMem.r.fire, p.dataBeats)

    mainMem.ar.bits := NastiAddressBundle(nasti)(
      0.U,
      (Cat(tag, idx) << p.offsetLen.U).asUInt,
      log2Up(nasti.dataBits / 8).U,
      (p.dataBeats - 1).U
    )

    //read address
    mainMem.ar.valid := false.B

    // read data
    mainMem.r.ready := false.B
    when(mainMem.r.fire) {
      buffer(read_count) := mainMem.r.bits.data
      bufferTag := tag
    }

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

    when(fsmHandle("sReadCache")) {
      printf("sReadCache\n")
      when(!hit) {
        mainMem.ar.valid := true.B
      }
    }

    when(fsmHandle("sRefill")) {
      printf("sRefill\n")
      mainMem.r.ready := true.B
    }

    fsmHandle("cleanMiss") := mainMem.ar.fire
    fsmHandle("refillFinish") := read_wrap_out
  }
}
