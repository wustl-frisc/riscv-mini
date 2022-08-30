
package mini

import chisel3._
import chisel3.util._
import chisel3.aop._
import chisel3.aop.injecting.InjectingAspect
import chisel3.aop.Select

import chisel3.experimental.BaseModule

object Features {
  val dirty = InjectingAspect(
    {top: Tile => Seq(top.dcache)},
    {thisJoinpoint: Cache =>  {
        val imageMem = Seq.fill(thisJoinpoint.nWords)(SyncReadMem(thisJoinpoint.nSets, Vec(thisJoinpoint.wBytes, UInt(8.W))))
        val idata = Cat((imageMem.map(_.read(thisJoinpoint.idx, thisJoinpoint.ren).asUInt)).reverse)
        when(thisJoinpoint.wen && thisJoinpoint.is_alloc) {
          imageMem.zipWithIndex.foreach {
          case (mem, i) =>
            val data = VecInit.tabulate(thisJoinpoint.wBytes)(k => thisJoinpoint.wdata(i * thisJoinpoint.xlen + (k + 1) * 8 - 1, i * thisJoinpoint.xlen + k * 8))
            mem.write(thisJoinpoint.idx_reg, data, thisJoinpoint.wmask((i + 1) * thisJoinpoint.wBytes - 1, i * thisJoinpoint.wBytes).asBools())
            mem.suggestName(s"dataMem_${i}")
          }
        }

        thisJoinpoint.is_dirty := thisJoinpoint.v(thisJoinpoint.idx_reg) && thisJoinpoint.d(thisJoinpoint.idx_reg) && (thisJoinpoint.rdata =/= idata)
      }
    }
  )
}