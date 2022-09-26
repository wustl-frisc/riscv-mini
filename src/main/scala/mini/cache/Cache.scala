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

  println("\tdata bits: "  + nasti.dataBits)

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
  println("\tword bytes: "+ wBytes)

  val byteOffsetBits = log2Ceil(wBytes)
  println("\tbyte offset bits: " + byteOffsetBits)

  val dataBeats = bBits / nasti.dataBits
  println("\tdata beats: " + dataBeats)
}


class Cache(val c: CacheConfig, val nasti: NastiBundleParameters, val xlen: Int) extends Module {
  // local parameters
  val p = new CacheParams(c.nSets, c.blockBytes, xlen, nasti)
  val cpu = IO(new CacheIO(xlen, xlen))
  val mainMem = IO(new NastiBundle(nasti))

  //this section is for things needed by the frontend and the backend
  //split the address up into the parts we need 
  private val nextAddress = Wire(chiselTypeOf(cpu.req.bits.addr))
  private val address = Reg(chiselTypeOf(nextAddress))
  private val tag = address(xlen - 1, p.indexLen + p.offsetLen)
  private val offset = address(p.offsetLen - 1, p.byteOffsetBits)

  //create a buffer for reads from main memory, also store the tag
  val buffer = Reg(Vec(p.dataBeats, UInt(nasti.dataBits.W)))

  //get the next address and mask from the datapath
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

  //create the base finite state machine
  private val sIdle = new IdleState("sIdle")
  private val sReadCache = new ReadState("sReadCache")
  private val sRefill = new MemoryState("sRefill")

  private val readReq = CacheToken("readReq")
  private val readFinish = CacheToken("readFinish")
  private val cleanMiss = CacheToken("cleanMiss")
  private val refillFinish = CacheToken("refillFinish")

  private val baseNFA = (new NFA(sIdle))
    .addTransition((sIdle, readReq), sReadCache)
    .addTransition((sReadCache, readFinish), sIdle)
    .addTransition((sReadCache, cleanMiss), sRefill)
    .addTransition((sRefill, refillFinish), sIdle)

  //we have to do a little extra here due to no middle
  def cache0() = {
    val bufferTag = Reg(UInt(p.tlen.W))

    val v = RegInit(false.B)
    hit := tag === bufferTag && v

    // Take the cache line and split it into its words, get the word we want
    val cacheLine = VecInit.tabulate(p.nWords)(i => buffer.asUInt((i + 1) * xlen - 1, i * xlen)) 
    val reqData = cacheLine(offset)
    
    val fsmHandle = ChiselFSMBuilder(baseNFA)
    val front = new Frontend(fsmHandle, p, cpu)
    front.read(hit)

    val back = new Backend(fsmHandle, p, mainMem, address)
    readDone := back.read(buffer, hit)
    back.writeStub()

    when(mainMem.r.fire) {
      bufferTag := tag
    }
    
    when(fsmHandle("sRefill")) {
      v := true.B
    }

    //give the CPU the data when it's ready
    cpu.resp.bits.data := reqData

    this
  }

  def readOnly() = {
    val fsmHandle = ChiselFSMBuilder(baseNFA)
    val front = new Frontend(fsmHandle, p, cpu)
    front.read(hit)

    val middle = new Middleend(fsmHandle, p, address)
    val valids = middle.read(buffer, nextAddress, tag, offset, hit, readDone, cpu)

    val back = new Backend(fsmHandle, p, mainMem, address)
    readDone := back.read(buffer, hit)
    back.writeStub()

    this
  }

  def writeBypass() = {
    val writeNFA = Weaver[NFA](List(new WriteBypass), baseNFA, (before: NFA, after: NFA) => before.isEqual(after))
    val fsmHandle = ChiselFSMBuilder(writeNFA)
    val front = new Frontend(fsmHandle, p, cpu)
    val middle = new Middleend(fsmHandle, p, address)
    val back = new Backend(fsmHandle, p, mainMem, address)

    front.read(hit)
    val (data, mask) = front.write(offset)

    val valids = middle.read(buffer, nextAddress, tag, offset, hit, readDone, cpu)

    readDone := back.read(buffer, hit)
    back.write(data, mask, offset, false.B)

    val index = address(p.indexLen + p.offsetLen - 1, p.offsetLen)
    when(fsmHandle("sWriteWait")) {
      valids := valids.bitSet(index, false.B)
    }

    this
  }

  def writeThrough() = {
    val writeNFA = Weaver[NFA](List(new WriteBypass), baseNFA, (before: NFA, after: NFA) => before.isEqual(after))
    val fsmHandle = ChiselFSMBuilder(writeNFA)
    val front = new Frontend(fsmHandle, p, cpu)
    val middle = new Middleend(fsmHandle, p, address)
    val back = new Backend(fsmHandle, p, mainMem, address)

    front.read(hit)
    val (data, mask) = front.write(offset)

    val valids = middle.read(buffer, nextAddress, tag, offset, hit, readDone, cpu)
    middle.write(data, mask, hit)

    readDone := back.read(buffer, hit)
    back.write(data, mask, offset, false.B)

    this
  }

}
