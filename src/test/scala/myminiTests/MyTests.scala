package myminiTests

import aoplib.{HistogramAspect, LoggingAspect, LoggingInfo}
import chisel3._
import mini._
import mymini.MyMiniHistogram


case object MyMiniTestHistogram extends HistogramAspect[TileTester] {
  override def histogramSignals(tester: TileTester): Seq[Data] = {
    tester.dut match {
      case dut: mini.Tile => MyMiniHistogram.histogramSignals(dut)
      case _ => Nil
    }
  }
}

case object MyMiniTestLogger extends LoggingAspect[TileTester] {
  override def logSignals(tester: TileTester): Seq[LoggingInfo] = {
    tester.dut match {
      case dut: mini.Tile =>
        //Seq(LoggingInfo(dut.core.dpath.csr.csrValid, Seq(dut.core.dpath.csr.csr_addr), dut.core.dpath.csr.clock, "csr_addr == %b\n"))
        //val alu = dut.core.dpath.alu
        injectTo(dut.core.dpath.alu) { alu =>
          printf("A == %d, B == %d, opcode == %d\n", Seq(alu.io.A, alu.io.B, alu.io.alu_op), alu.clock, Some(alu.io.alu_op === 0.U))
        }
        Seq(LoggingInfo("A == %d, B == %d, opcode == %d\n", Seq(alu.io.A, alu.io.B, alu.io.alu_op), alu.clock, Some(alu.io.alu_op === 0.U)))
      case _ => Nil
    }
  }
}

class TileSimpleTests extends TileTests(SimpleTests, aspects = Seq(MyMiniTestLogger))
