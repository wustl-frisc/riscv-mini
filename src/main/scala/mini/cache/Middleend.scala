package mini
package cache

import chisel3._
import chisel3.util._
import junctions._
import foam._

class Middleend(fsmHandle: ChiselFSMHandle, p: CacheParams, address: UInt, tag: UInt, index: UInt) {
  //set up the bookkeeping
  private val valids = RegInit(0.U(p.nSets.W))
  //just store the tags
  private val tags = SyncReadMem(p.nSets, UInt(p.tlen.W))
  //for each set, we have a line that is nWords * wBytes long
  private val localMemory = SyncReadMem(p.nSets, Vec(p.nWords * p.wBytes, UInt((p.byteBits).W)))

  private val allocateCond = Wire(Bool())
  allocateCond := false.B

  private val updateCond = Wire(Bool())
  updateCond := false.B

  private val readEnable = Wire(Bool())
  readEnable := !(allocateCond || updateCond)

  def read(buffer: Vec[UInt], nextAddress: UInt, offset: UInt, hit: Option[Bool] = None, cpu: Option[CacheIO] = None) = {

    //the index of the next address
    val nextIndex = nextAddress(p.indexLen + p.offsetLen - 1, p.offsetLen)
    //the tag of the next address
    //tags requires 1 clock cycle to return data, thus we ask for the next index which
    //will be the current one when the data is ready
    val nextTag = tags.read(nextIndex)

    //the hit is when the line is valid and we have it in cache
    hit match {
      case Some(hit) => hit := valids(index) && nextTag === tag
      case None =>
    }

    //The writes take effect on the cycle after the request, need to switch to the buffer for 1 cycle.
    val justAllocated = RegNext(allocateCond)

    //Take the cache line and split it into its words, get the word we want
    val readPort =
      localMemory.read(nextIndex, readEnable) //get started on the next PC, will take 1 clock cycle to get the data
    val readData =
      Mux(justAllocated, buffer.asUInt, readPort.asUInt) //read from the buffer until we're sure the write is finished
    val cacheLine = VecInit.tabulate(p.nWords)(i => readData((i + 1) * p.xlen - 1, i * p.xlen))
    val reqData = cacheLine(offset)

    cpu match {
      case Some(io) => io.resp.bits.data := reqData
      case None => 
    }

    (valids, nextTag, readData)
  }

  def allocate(cacheLine: UInt, allocateCond: Bool) = {
    this.allocateCond := allocateCond
    //if we're executing this, the *current pc* was a miss, write into the local memory using the current index
    when(allocateCond) {
      valids := valids.bitSet(index, true.B)
      tags.write(index, tag)
      val writeVec =
        VecInit.tabulate(p.nWords * p.wBytes)(i => cacheLine.asUInt((i + 1) * p.byteBits - 1, i * p.byteBits))
      localMemory.write(index, writeVec)
    }
  }

  def update(data: UInt, mask: Vec[UInt], updateCond: Bool) = {
    this.updateCond := updateCond

    when(updateCond) {
      val writeVec = VecInit.tabulate(p.nWords * p.wBytes)(i => data.asUInt((i + 1) * p.byteBits - 1, i * p.byteBits))
      localMemory.write(index, writeVec, mask.asUInt.asBools)
    }
  }

}
