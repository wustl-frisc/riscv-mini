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

class CacheModuleIO(nastiParams: NastiBundleParameters, addrWidth: Int, dataWidth: Int) extends Bundle {
  val cpu = new CacheIO(addrWidth, dataWidth)
  val nasti = new NastiBundle(nastiParams)
}

case class CacheConfig(nWays: Int, nSets: Int, blockBytes: Int)

class MetaData(tagLength: Int) extends Bundle {
  val tag = UInt(tagLength.W)
}

class CacheParams(val nSets: Int, blockBytes: Int, xlen: Int, dataBits: Int) {
  val bBytes = blockBytes
  val bBits = bBytes << 3
  val blen = log2Ceil(bBytes)
  val slen = log2Ceil(nSets)
  val tlen = xlen - (slen + blen)
  val nWords = bBits / xlen
  val wBytes = xlen / 8
  val byteOffsetBits = log2Ceil(wBytes)
  val dataBeats = bBits / dataBits
}


class Cache(val c: CacheConfig, val nasti: NastiBundleParameters, val xlen: Int) extends Module {
  // local parameters
  val p = new CacheParams(c.nSets, c.blockBytes, xlen, nasti.dataBits)

  val io = IO(new CacheModuleIO(nasti, addrWidth = xlen, dataWidth = xlen))

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

  // memory
  val v = RegInit(0.U(p.nSets.W))
  val metaMem = SyncReadMem(p.nSets, new MetaData(p.tlen))
  val dataMem = Seq.fill(p.nWords)(SyncReadMem(p.nSets, Vec(p.wBytes, UInt(8.W))))

  val addr_reg = Reg(chiselTypeOf(io.cpu.req.bits.addr))
  val cpu_data = Reg(chiselTypeOf(io.cpu.req.bits.data))
  val cpu_mask = Reg(chiselTypeOf(io.cpu.req.bits.mask))

  // Counters
  require(p.dataBeats > 0)
  val (read_count, read_wrap_out) = Counter(io.nasti.r.fire, p.dataBeats)

  val is_alloc = Wire(Bool()) //state === sRefill && read_wrap_out
  is_alloc := false.B
  val is_alloc_reg = RegNext(is_alloc)

  val hit = Wire(Bool())
  val wen = Wire(Bool())
  wen := is_alloc

  val ren = Wire(Bool()) 
  ren := false.B
  val ren_reg = RegNext(ren)

  val addr = io.cpu.req.bits.addr
  val idx = addr(p.slen + p.blen - 1, p.blen)
  val tag_reg = addr_reg(xlen - 1, p.slen + p.blen)
  val idx_reg = addr_reg(p.slen + p.blen - 1, p.blen)
  val off_reg = addr_reg(p.blen - 1, p.byteOffsetBits)

  val rmeta = metaMem.read(idx, ren)
  val rdata = Cat((dataMem.map(_.read(idx, ren).asUInt)).reverse)
  val rdata_buf = RegEnable(rdata, ren_reg)
  val refill_buf = Reg(Vec(p.dataBeats, UInt(nasti.dataBits.W)))
  val read = Mux(is_alloc_reg, refill_buf.asUInt, Mux(ren_reg, rdata, rdata_buf))

  hit := v(idx_reg) && rmeta.tag === tag_reg

  // Read Mux
  io.cpu.resp.bits.data := VecInit.tabulate(p.nWords)(i => read((i + 1) * xlen - 1, i * xlen))(off_reg)
  io.cpu.resp.valid := is_alloc_reg && !cpu_mask.orR

  when(io.cpu.resp.valid) {
    addr_reg := addr
    cpu_data := io.cpu.req.bits.data
    cpu_mask := io.cpu.req.bits.mask
  }

  val wmeta = Wire(new MetaData(p.tlen))
  wmeta.tag := tag_reg

  val wmask = -1.S
  val wdata = if (refill_buf.size == 1) io.nasti.r.bits.data
    else Cat(io.nasti.r.bits.data, Cat(refill_buf.init.reverse))

  when(wen) {
    v := v.bitSet(idx_reg, true.B)
    metaMem.write(idx_reg, wmeta)
    dataMem.zipWithIndex.foreach {
      case (mem, i) =>
        val data = VecInit.tabulate(p.wBytes)(k => wdata(i * xlen + (k + 1) * 8 - 1, i * xlen + k * 8))
        mem.write(idx_reg, data, wmask((i + 1) * p.wBytes - 1, i * p.wBytes).asBools())
        mem.suggestName(s"dataMem_${i}")
    }
  }

  io.nasti.ar.bits := NastiAddressBundle(nasti)(
    0.U,
    (Cat(tag_reg, idx_reg) << p.blen.U).asUInt,
    log2Up(nasti.dataBits / 8).U,
    (p.dataBeats - 1).U
  )
  io.nasti.ar.valid := false.B
  // read data
  io.nasti.r.ready := false.B
  when(io.nasti.r.fire) {
    refill_buf(read_count) := io.nasti.r.bits.data
  }

  // write addr
  io.nasti.aw.bits := NastiAddressBundle(nasti)(
    0.U,
    0.U,
    0.U,
    0.U
  )
  io.nasti.aw.valid := false.B
  // write data
  io.nasti.w.bits := NastiWriteDataBundle(nasti)(
    0.U,
    None,
    false.B
  )
  io.nasti.w.valid := false.B
  // write resp
  io.nasti.b.ready := false.B

  // Cache FSM
  val is_dirty = Wire(Bool())
  is_dirty := false.B

  when(fsmHandle("sIdle")) {
    ren := !wen && io.cpu.req.valid
    io.cpu.resp.valid := true.B
  }

  when(fsmHandle("sReadCache")) {
    ren := !wen && io.cpu.req.valid

    when(hit){
      io.cpu.resp.valid := true.B
    }.otherwise {
      io.nasti.ar.valid := !is_dirty
    }
  }

  when(fsmHandle("sRefill")) {
    io.nasti.r.ready := true.B
    when(read_wrap_out) {
        is_alloc := true.B
      }
  }

  fsmHandle("readReq") := io.cpu.req.valid && !io.cpu.req.bits.mask.orR
  fsmHandle("readHit") := hit && io.cpu.req.valid && !io.cpu.req.bits.mask.orR
  fsmHandle("readFinish") := hit && !io.cpu.req.valid
  fsmHandle("cleanMiss") := !hit && io.nasti.ar.fire
  fsmHandle("refillFinish") := read_wrap_out && !cpu_mask.orR
}
