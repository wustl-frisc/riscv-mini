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

class InstructionCache(c: CacheConfig, nasti: NastiBundleParameters, xlen: Int) extends Cache(c, nasti, xlen)
  with HasCleanRead with HasWriteStub

class DataCache(c: CacheConfig, nasti: NastiBundleParameters, xlen: Int) extends Cache(c, nasti, xlen) with HasWriteNFA with HasCleanRead with HasSimpleWrite

class Cache(val c: CacheConfig, val nasti: NastiBundleParameters, val xlen: Int) extends Module {
  //outward facing IO
  val cpu = IO(new CacheIO(xlen, xlen))
  val mainMem = IO(new NastiBundle(nasti))

  // local parameters
  protected val p = new CacheParams(c.nSets, c.blockBytes, xlen, nasti)

  //build the NFA for the cache
  protected lazy val cacheNFA = Weaver[NFA](List(), ReadFSM(), (before: NFA, after: NFA) => before.isEqual(after))
  protected lazy val fsmHandle = ChiselFSMBuilder(cacheNFA)

  //this section is for things needed by the frontend and the backend
  //split the address up into the parts we need
  protected val nextAddress = Wire(chiselTypeOf(cpu.req.bits.addr))
  protected val address = Reg(chiselTypeOf(nextAddress))
  protected val tag = address(xlen - 1, p.indexLen + p.offsetLen)
  protected val offset = address(p.offsetLen - 1, p.byteOffsetBits)
  protected val index = if (p.nSets == 1) 0.U else address(p.indexLen + p.offsetLen - 1, p.offsetLen)

  //create a buffer for reads from main memory, also store the tag
  protected val buffer = Reg(Vec(p.dataBeats, UInt(nasti.dataBits.W)))
  //we will always have at least this
  val valids = RegInit(0.U(p.nSets.W))

  //set up phases used in every cache
  protected val front = new Frontend(fsmHandle, p, cpu)
  protected val back = new Backend(fsmHandle, p, mainMem, address)

  //hit can be several different things depending on the feature set
  protected val hit = Wire(Bool())
  hit := false.B

  protected val readDone = Wire(Bool())
  readDone := false.B

  //get the next address from the datapath
  nextAddress := cpu.req.bits.addr
  cpu.resp.valid := false.B

  //after we've got a valid req, we can move onto the next req
  when(cpu.resp.valid) {
    address := nextAddress
  }

  front.read(hit)

  //provide default values
  val dirtyRead = Wire(Bool())
  dirtyRead := false.B

  val isRead = Wire(Bool())
  isRead := true.B

  val writeDone = Wire(Bool())
  writeDone := false.B

  val localWrite = Wire(Bool())
  localWrite := cpu.abort
  
  val dirtyWrite = Wire(Bool())
  dirtyWrite := true.B
}
