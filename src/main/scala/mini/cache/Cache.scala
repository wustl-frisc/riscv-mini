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


class Cache(val p: CacheConfig, val nasti: NastiBundleParameters, val xlen: Int) extends Module {
  // local parameters
  val nSets = p.nSets
  val bBytes = p.blockBytes
  val bBits = bBytes << 3
  val blen = log2Ceil(bBytes)
  val slen = log2Ceil(nSets)
  val tlen = xlen - (slen + blen)
  val nWords = bBits / xlen
  val wBytes = xlen / 8
  val byteOffsetBits = log2Ceil(wBytes)
  val dataBeats = bBits / nasti.dataBits

  val io = IO(new CacheModuleIO(nasti, addrWidth = xlen, dataWidth = xlen))

  // memory
  val v = RegInit(0.U(nSets.W))
  val d = RegInit(0.U(nSets.W))
  val metaMem = SyncReadMem(nSets, new MetaData(tlen))
  val dataMem = Seq.fill(nWords)(SyncReadMem(nSets, Vec(wBytes, UInt(8.W))))

  val addr_reg = Reg(chiselTypeOf(io.cpu.req.bits.addr))
  val cpu_data = Reg(chiselTypeOf(io.cpu.req.bits.data))
  val cpu_mask = Reg(chiselTypeOf(io.cpu.req.bits.mask))

  // Counters
  require(dataBeats > 0)
  val (read_count, read_wrap_out) = Counter(io.nasti.r.fire, dataBeats)
  val (write_count, write_wrap_out) = Counter(io.nasti.w.fire, dataBeats)

  val is_idle = Wire(Bool())
  is_idle := false.B
  val is_read = Wire(Bool())
  is_read := false.B
  val is_write = Wire(Bool())
  is_write := false.B
  val is_alloc = Wire(Bool()) //state === sRefill && read_wrap_out
  is_alloc := false.B
  val is_alloc_reg = RegNext(is_alloc)

  val hit = Wire(Bool())
  val wen = is_write && (hit || is_alloc_reg) && !io.cpu.abort || is_alloc
  val ren = !wen && (is_idle || is_read) && io.cpu.req.valid
  val ren_reg = RegNext(ren)

  val addr = io.cpu.req.bits.addr
  val idx = addr(slen + blen - 1, blen)
  val tag_reg = addr_reg(xlen - 1, slen + blen)
  val idx_reg = addr_reg(slen + blen - 1, blen)
  val off_reg = addr_reg(blen - 1, byteOffsetBits)

  val rmeta = metaMem.read(idx, ren)
  val rdata = Cat((dataMem.map(_.read(idx, ren).asUInt)).reverse)
  val rdata_buf = RegEnable(rdata, ren_reg)
  val refill_buf = Reg(Vec(dataBeats, UInt(nasti.dataBits.W)))
  val read = Mux(is_alloc_reg, refill_buf.asUInt, Mux(ren_reg, rdata, rdata_buf))

  hit := v(idx_reg) && rmeta.tag === tag_reg

  // Read Mux
  io.cpu.resp.bits.data := VecInit.tabulate(nWords)(i => read((i + 1) * xlen - 1, i * xlen))(off_reg)
  io.cpu.resp.valid := is_idle || is_read && hit || is_alloc_reg && !cpu_mask.orR

  when(io.cpu.resp.valid) {
    addr_reg := addr
    cpu_data := io.cpu.req.bits.data
    cpu_mask := io.cpu.req.bits.mask
  }

  val wmeta = Wire(new MetaData(tlen))
  wmeta.tag := tag_reg

  val wmask = Mux(!is_alloc, (cpu_mask << Cat(off_reg, 0.U(byteOffsetBits.W))).zext, (-1).S)
  val wdata = Mux(
    !is_alloc,
    Fill(nWords, cpu_data),
    if (refill_buf.size == 1) io.nasti.r.bits.data
    else Cat(io.nasti.r.bits.data, Cat(refill_buf.init.reverse))
  )

  when(wen) {
    v := v.bitSet(idx_reg, true.B)
    d := d.bitSet(idx_reg, !is_alloc)
    when(is_alloc) {
      metaMem.write(idx_reg, wmeta)
    }
    dataMem.zipWithIndex.foreach {
      case (mem, i) =>
        val data = VecInit.tabulate(wBytes)(k => wdata(i * xlen + (k + 1) * 8 - 1, i * xlen + k * 8))
        mem.write(idx_reg, data, wmask((i + 1) * wBytes - 1, i * wBytes).asBools())
        mem.suggestName(s"dataMem_${i}")
    }
  }

  io.nasti.ar.bits := NastiAddressBundle(nasti)(
    0.U,
    (Cat(tag_reg, idx_reg) << blen.U).asUInt,
    log2Up(nasti.dataBits / 8).U,
    (dataBeats - 1).U
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
    (Cat(rmeta.tag, idx_reg) << blen.U).asUInt,
    log2Up(nasti.dataBits / 8).U,
    (dataBeats - 1).U
  )
  io.nasti.aw.valid := false.B
  // write data
  io.nasti.w.bits := NastiWriteDataBundle(nasti)(
    VecInit.tabulate(dataBeats)(i => read((i + 1) * nasti.dataBits - 1, i * nasti.dataBits))(write_count),
    None,
    write_wrap_out
  )
  io.nasti.w.valid := false.B
  // write resp
  io.nasti.b.ready := false.B

  // Cache FSM
  val is_dirty = v(idx_reg) && d(idx_reg)

  val sIdle = CacheStateFactory({
    is_idle := true.B
  })
  val sReadCache = CacheStateFactory({
    is_read := true.B
      when(!hit){
        io.nasti.aw.valid := is_dirty
        io.nasti.ar.valid := !is_dirty
      }
  })
  val sWriteCache = CacheStateFactory({
    is_write := true.B
    when(!(hit || is_alloc_reg || io.cpu.abort)) {
      io.nasti.aw.valid := is_dirty
      io.nasti.ar.valid := !is_dirty
    }
  })
  val sWriteBack = CacheStateFactory({
    io.nasti.w.valid := true.B
  })
  val sWriteAck = CacheStateFactory({
    io.nasti.b.ready := true.B
  })
  val sRefillReady = CacheStateFactory({
    io.nasti.ar.valid := true.B
  })
  val sRefill = CacheStateFactory({
    io.nasti.r.ready := true.B
    when(read_wrap_out) {
        is_alloc := true.B
      }
  })

  val readReq = CacheToken(io.cpu.req.valid && !io.cpu.req.bits.mask.orR)
  val writeReq = CacheToken(io.cpu.req.valid && io.cpu.req.bits.mask.orR)
  val readHit = CacheToken(hit && io.cpu.req.valid && !io.cpu.req.bits.mask.orR)
  val writeHit = CacheToken(hit && io.cpu.req.valid && io.cpu.req.bits.mask.orR)
  val readFinish = CacheToken(hit && !io.cpu.req.valid)
  val dirtyMiss = CacheToken(!hit && io.nasti.aw.fire)
  val cleanMiss = CacheToken(!hit && io.nasti.ar.fire)
  val writeFinish = CacheToken(hit || is_alloc_reg || io.cpu.abort)
  val ack = CacheToken(write_wrap_out)
  val refillReady = CacheToken(io.nasti.b.fire)
  val doRefill = CacheToken(io.nasti.ar.fire)
  val refillFinish = CacheToken(read_wrap_out && !cpu_mask.orR)
  val doWrite = CacheToken(read_wrap_out && cpu_mask.orR)

  val cacheNFA = (new NFA(sIdle))
    .addTransition((sIdle, readReq), sReadCache)
    .addTransition((sIdle, writeReq), sWriteCache)
    .addTransition((sReadCache, readHit), sReadCache)
    .addTransition((sReadCache, readFinish), sIdle)
    .addTransition((sReadCache, dirtyMiss), sWriteBack)
    .addTransition((sReadCache, cleanMiss), sRefill)
    .addTransition((sReadCache, writeHit), sWriteCache)
    .addTransition((sWriteCache, writeFinish), sIdle)
    .addTransition((sWriteCache, dirtyMiss), sWriteBack)
    .addTransition((sWriteCache, cleanMiss), sRefill)
    .addTransition((sWriteBack, ack), sWriteAck)
    .addTransition((sWriteAck, refillReady), sRefillReady)
    .addTransition((sRefillReady, doRefill), sRefill)
    .addTransition((sRefill, refillFinish), sIdle)
    .addTransition((sRefill, doWrite), sWriteCache)

  ChiselFSMBuilder(cacheNFA)
}
