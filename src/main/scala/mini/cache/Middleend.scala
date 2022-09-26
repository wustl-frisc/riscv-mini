package mini
package cache

import chisel3._
import chisel3.util._
import junctions._
import foam._

class Middleend(fsmHandle: ChiselFSMHandle, p: CacheParams, address: UInt) {
    //set up the bookkeeping
    val valids = RegInit(0.U(p.nSets.W))
    //just store the tags
    val tags = SyncReadMem(p.nSets, UInt(p.tlen.W))
    //for each set, we have a line that is nWords * wBytes long
    val localMemory = SyncReadMem(p.nSets, Vec(p.nWords * p.wBytes, UInt((p.byteBits).W)))
    //the index of the current address
    val index = address(p.indexLen + p.offsetLen - 1, p.offsetLen)

    def read(buffer: Vec[UInt], nextAddress: UInt, tag: UInt, offset: UInt, hit: Bool, 
      readDone: Bool, io: CacheIO) = {

      //the index of the next address
      val nextIndex = nextAddress(p.indexLen + p.offsetLen - 1, p.offsetLen)
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
          val writeVec = VecInit.tabulate(p.nWords * p.wBytes)(i => buffer.asUInt((i + 1) * p.byteBits - 1, i * p.byteBits))
          localMemory.write(index, writeVec)
      }

      //Take the cache line and split it into its words, get the word we want
      val readPort = localMemory.read(nextIndex) //get started on the next PC, will take 1 clock cycle to get the data
      val readData = Mux(readJustDone, buffer.asUInt, readPort.asUInt) //read from the buffer until we're sure the write is finished
      val cacheLine = VecInit.tabulate(p.nWords)(i => readData((i + 1) * p.xlen - 1, i * p.xlen)) 
      val reqData = cacheLine(offset)

      //connect our data lines to the datapath
      io.resp.bits.data := reqData

      valids
    }

    def write(data: UInt, mask: Vec[UInt], hit: Bool) = {
      when(fsmHandle("sWriteCache")) {
        when(hit) {
          val writeVec = VecInit.tabulate(p.nWords * p.wBytes)(i => data.asUInt((i + 1) * p.byteBits - 1, i * p.byteBits))
          localMemory.write(index, writeVec, mask.asUInt.asBools)
        }
      }
    }

  }