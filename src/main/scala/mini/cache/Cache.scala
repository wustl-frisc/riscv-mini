// See LICENSE for license details.

package mini
package cache

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import junctions._
import foam._
import foam.aspects._

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

class CacheParams(val nSets: Int, val blockBytes: Int, val xlen: Int, val nasti: NastiBundleParameters) {
  println("Cache Config:")

  println("\tdata bits: " + nasti.dataBits)

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

  val byteBits = 8
  println("\tbyte bits: " + byteBits)

  val wBytes = xlen / byteBits
  println("\tword bytes: " + wBytes)

  val byteOffsetBits = log2Ceil(wBytes)
  println("\tbyte offset bits: " + byteOffsetBits)

  val dataBeats = bBits / nasti.dataBits
  println("\tdata beats: " + dataBeats)
}

class Cache(val c: CacheConfig, val nasti: NastiBundleParameters, val xlen: Int) extends Module {
  //outward facing IO
  val cpu = IO(new CacheIO(xlen, xlen))
  val mainMem = IO(new NastiBundle(nasti))

  // local parameters
  private val p = new CacheParams(c.nSets, c.blockBytes, xlen, nasti)

  //this section is for things needed by the frontend and the backend
  //split the address up into the parts we need
  private val nextAddress = Wire(chiselTypeOf(cpu.req.bits.addr))
  private val address = Reg(chiselTypeOf(nextAddress))
  private val tag = address(xlen - 1, p.indexLen + p.offsetLen)
  private val offset = address(p.offsetLen - 1, p.byteOffsetBits)
  private val index = if (p.nSets == 1) 0.U else address(p.indexLen + p.offsetLen - 1, p.offsetLen)

  //create a buffer for reads from main memory, also store the tag
  private val buffer = Reg(Vec(p.dataBeats, UInt(nasti.dataBits.W)))

  //get the next address from the datapath
  nextAddress := cpu.req.bits.addr
  cpu.resp.valid := false.B

  //after we've got a valid req, we can move onto the next req
  when(cpu.resp.valid) {
    address := nextAddress
  }

  //hit can be several different things depending on the feature set
  private val hit = Wire(Bool())
  hit := false.B

  private val readDone = Wire(Bool())
  readDone := false.B

  //we have to do a little extra here due to no middle
  def cache0() = {
    val fsmHandle = ChiselFSMBuilder(ReadFSM())
    val front = new Frontend(fsmHandle, p, cpu)
    val back = new Backend(fsmHandle, p, mainMem, address)

    //simple bookkeeping
    val bufferTag = Reg(UInt(p.tlen.W))
    val v = RegInit(false.B)
    hit := tag === bufferTag && v

    // Take the cache line and split it into its words, get the word we want
    val cacheLine = VecInit.tabulate(p.nWords)(i => buffer.asUInt((i + 1) * xlen - 1, i * xlen))
    val reqData = cacheLine(offset)

    front.read(hit)

    readDone := back.read((address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt, buffer, hit)
    back.writeStub()

    //update our simple bookkeeping
    when(mainMem.r.fire) {
      bufferTag := tag
    }

    //have to set this to true to keep the processor executing
    when(fsmHandle("sRefill")) {
      v := true.B
    }

    //give the CPU the data when it's ready
    cpu.resp.bits.data := reqData

    this
  }

  def cache1() = {
    val writeNFA =
      Weaver[NFA](List(new AckIdle), ReadFSM() + WriteFSM(), (before: NFA, after: NFA) => before.isEqual(after))
    val fsmHandle = ChiselFSMBuilder(writeNFA)
    val front = new Frontend(fsmHandle, p, cpu)
    val back = new Backend(fsmHandle, p, mainMem, address)

    //simple bookkeeping
    val bufferTag = Reg(UInt(p.tlen.W))
    val v = RegInit(false.B)
    hit := tag === bufferTag && v

    // Take the cache line and split it into its words, get the word we want
    val cacheLine = VecInit.tabulate(p.nWords)(i => buffer.asUInt((i + 1) * xlen - 1, i * xlen))
    val reqData = cacheLine(offset)

    front.read(hit)
    val (data, mask) = front.write(offset)

    readDone := back.read((address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt, buffer, hit)
    back.write((address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt, data, Some(mask), offset, cpu.abort)

    //update our simple bookkeeping
    when(mainMem.r.fire) {
      bufferTag := tag
    }

    //have to set this to true to keep the processor executing
    when(fsmHandle("sRefill")) {
      v := true.B
    }

    when(fsmHandle("sWriteWait")) {
      v := false.B
    }

    //give the CPU the data when it's ready
    cpu.resp.bits.data := reqData

    this
  }

  def readOnly() = {
    val fsmHandle = ChiselFSMBuilder(ReadFSM())
    val front = new Frontend(fsmHandle, p, cpu)
    val middle = new Middleend(fsmHandle, p, address, tag, index)
    val back = new Backend(fsmHandle, p, mainMem, address)

    front.read(hit)

    val (valids, _, _) = middle.read(buffer, nextAddress, offset, hit, cpu)
    middle.allocate(Cat(mainMem.r.bits.data, Cat(buffer.init.reverse)), readDone)

    readDone := back.read((address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt, buffer, hit)
    back.writeStub()

    this
  }

  def writeBypass() = {
    val writeNFA =
      Weaver[NFA](List(new AckIdle), ReadFSM() + WriteFSM(), (before: NFA, after: NFA) => before.isEqual(after))
    val fsmHandle = ChiselFSMBuilder(writeNFA)
    val front = new Frontend(fsmHandle, p, cpu)
    val middle = new Middleend(fsmHandle, p, address, tag, index)
    val back = new Backend(fsmHandle, p, mainMem, address)

    front.read(hit)
    val (data, mask) = front.write(offset)

    val (valids, _, _) = middle.read(buffer, nextAddress, offset, hit, cpu)
    middle.allocate(Cat(mainMem.r.bits.data, Cat(buffer.init.reverse)), readDone)

    readDone := back.read((address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt, buffer, hit)
    back.write(
      (address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt,
      data,
      Some(mask),
      offset,
      cpu.abort
    ) //only write when we're not doing an abort

    when(fsmHandle("sWriteWait")) {
      valids := valids.bitSet(index, false.B)
    }

    this
  }

  def writeThrough() = {
    val writeNFA =
      Weaver[NFA](List(new AckIdle), ReadFSM() + WriteFSM(), (before: NFA, after: NFA) => before.isEqual(after))
    val fsmHandle = ChiselFSMBuilder(writeNFA)
    val front = new Frontend(fsmHandle, p, cpu)
    val middle = new Middleend(fsmHandle, p, address, tag, index)
    val back = new Backend(fsmHandle, p, mainMem, address)

    front.read(hit)
    val (data, mask) = front.write(offset)

    val (valids, _, _) = middle.read(buffer, nextAddress, offset, hit, cpu)
    middle.allocate(Cat(mainMem.r.bits.data, Cat(buffer.init.reverse)), readDone)
    middle.update(data, mask, fsmHandle("sWriteCache") && hit && !cpu.abort)

    readDone := back.read((address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt, buffer, hit)
    back.write((address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt, data, Some(mask), offset, cpu.abort)

    this
  }

  def writeBack() = {
    val writeBackNFA =
      (Weaver[NFA](
        List(new AckRead, new DirtyAccounting),
        ReadFSM() + WriteFSM(),
        (before: NFA, after: NFA) => before.isEqual(after)
      ))

    val fsmHandle = ChiselFSMBuilder(writeBackNFA)
    val front = new Frontend(fsmHandle, p, cpu)
    val middle = new Middleend(fsmHandle, p, address, tag, index)
    val back = new Backend(fsmHandle, p, mainMem, address)

    val readJustDone = RegNext(readDone)
    val dirty = RegInit(0.U(p.nSets.W))

    front.read(hit)
    val (data, mask) = front.write(offset, hit || readJustDone)

    val (valids, oldTag, readData) = middle.read(buffer, nextAddress, offset, hit, cpu)
    middle.allocate(Cat(mainMem.r.bits.data, Cat(buffer.init.reverse)), readDone)
    val updateCond = fsmHandle("sWriteCache") && (hit || readJustDone) && !cpu.abort
    middle.update(data, mask, updateCond)

    val localWrite = hit || readJustDone || cpu.abort
    readDone := back.read(
      (address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt,
      buffer,
      hit,
      dirty(index),
      !mask.asUInt.orR
    )
    back.write((Cat(oldTag, index) << p.offsetLen.U).asUInt, readData, None, offset, localWrite, dirty(index))

    when(updateCond) {
      dirty := dirty.bitSet(index, true.B)
    }

    when(fsmHandle("sWriteWait")) {
      dirty := dirty.bitSet(index, false.B)
    }

    fsmHandle("doWrite") := mask.asUInt.orR && readDone
    fsmHandle("dirtyMiss") := !hit && dirty(index)
    fsmHandle("cleanMiss") := !localWrite && !dirty(index)

    this
  }

}
