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

class CacheParams(val nSets: Int, blockBytes: Int, xlen: Int, dataBits: Int) {
  println("Cache Config:")

  println("\tdata bits: "  + dataBits)

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

  val dataBeats = bBits / dataBits
  println("\tdata beats: " + dataBeats)
}


class Cache(val c: CacheConfig, val nasti: NastiBundleParameters, val xlen: Int) extends Module {
  // local parameters
  val p = new CacheParams(c.nSets, c.blockBytes, xlen, nasti.dataBits)
  val cpu = IO(new CacheIO(xlen, xlen))
  val mainMem = IO(new NastiBundle(nasti))

  //this section is for things needed by the frontend and the backend
  //split the address up into the parts we need
  //TODO: Move what we can into constructors of classes 
  private[cache] val nextAddress = Wire(chiselTypeOf(cpu.req.bits.addr))
  private[cache] val address = Reg(chiselTypeOf(nextAddress))
  private[cache] val tag = address(xlen - 1, p.indexLen + p.offsetLen)
  private[cache] val offset = address(p.offsetLen - 1, p.byteOffsetBits)
  val valids = RegInit(0.U(p.nSets.W))

  //create a buffer for reads from main memory, also store the tag
  private val buffer = Reg(Vec(p.dataBeats, UInt(nasti.dataBits.W)))
  private val bufferTag = Reg(UInt(p.tlen.W))
  //hit can be several different things depending on the feature set
  private[cache] val hit = Wire(Bool())
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

  def cache0() = {
    val v = RegInit(false.B)
    hit := tag === bufferTag && v

    // Take the cache line and split it into its words, get the word we want
    val cacheLine = VecInit.tabulate(p.nWords)(i => buffer.asUInt((i + 1) * xlen - 1, i * xlen)) 
    val reqData = cacheLine(offset)

    //give the CPU the data when it's ready
    cpu.resp.bits.data := reqData
    
    val fsmHandle = ChiselFSMBuilder(baseNFA)
    val front = new Frontend(this, fsmHandle)
    front.read()

    val back = new Backend(this, fsmHandle)
    readDone := back.read(buffer, bufferTag)
    back.writeStub()
    
    when(fsmHandle("sRefill")) {
      v := true.B
    }

    this
  }

  def readOnly() = {
    val fsmHandle = ChiselFSMBuilder(baseNFA)
    val front = new Frontend(this, fsmHandle)
    front.read()

    middleend(fsmHandle)

    val back = new Backend(this, fsmHandle)
    readDone := back.read(buffer, bufferTag)
    back.writeStub()

    this
  }

  def writeBypass() = {
    val writeNFA = Weaver[NFA](List(new WriteBypass), baseNFA, (before: NFA, after: NFA) => before.isEqual(after))
    val fsmHandle = ChiselFSMBuilder(writeNFA)
    val front = new Frontend(this, fsmHandle)
    front.read()
    front.write()

    middleend(fsmHandle)

    val data = Reg(chiselTypeOf(cpu.req.bits.data))
    val mask = Reg(chiselTypeOf(cpu.req.bits.data))

    when(cpu.resp.valid) {
      data := cpu.req.bits.data
      mask := cpu.req.bits.mask
    }

    val back = new Backend(this, fsmHandle)
    readDone := back.read(buffer, bufferTag)
    back.sparceWrite(data, mask)

    this
  }

  private def middleend(fsmHandle: ChiselFSMHandle) = {
    //set up the bookkeeping
    //just store the tags
    val tags = SyncReadMem(p.nSets, UInt(p.tlen.W))
    //for each set, we have a line that is nWords * wBytes long
    val localMemory = SyncReadMem(p.nSets, UInt((p.nWords * p.wBytes * p.byteBits).W))

    //the index of the next address
    val nextIndex = nextAddress(p.indexLen + p.offsetLen - 1, p.offsetLen)
    //the index of the current address
    val index = address(p.indexLen + p.offsetLen - 1, p.offsetLen)
    //the tag of the next address
    //tags requires 1 clock cycle to return data, thus we ask for the next index which
    //will be the current one when the data is ready
    val nextTag = tags.read(nextIndex).asUInt

    //the hit is when the line is valid and we have it in cache
    hit := valids(index) && nextTag === tag

    //it takes two cycles for the cache line to be written, we have to account for that here
    val readJustDone = RegNext(readDone)
    //if we're executing this, the *current pc* was a miss, write into the local memory using the current index
    when(readDone || readJustDone) {
        valids := valids.bitSet(index, true.B)
        tags.write(index, tag)
        localMemory.write(index, buffer.asUInt)
    }

    //Take the cache line and split it into its words, get the word we want
    val readPort = localMemory.read(nextIndex).asUInt //get started on the next PC, will take 1 clock cycle to get the data
    val readData = Mux(readJustDone, buffer.asUInt, readPort) //read from the buffer until we're sure the write is finished
    val cacheLine = VecInit.tabulate(p.nWords)(i => readData((i + 1) * xlen - 1, i * xlen)) 
    val reqData = cacheLine(offset)

    //connect our data lines to the datapath
    cpu.resp.bits.data := reqData
  }
}
