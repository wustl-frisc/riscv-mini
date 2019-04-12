package myminiTests

//import aoplib.{HistogramAspect, LoggingAspect, LoggingInfo}
import chisel3._
import chisel3.aop.{Concern, InjectingAspect}
import mini._
//import mymini.MyMiniHistogram


//case object MyMiniTestHistogram extends HistogramAspect[TileTester] {
//  override def histogramSignals(tester: TileTester): Seq[Data] = {
//    tester.dut match {
//      case dut: mini.Tile => MyMiniHistogram.histogramSignals(dut)
//      case _ => Nil
//    }
//  }
//}

case object MyMiniTestLogger extends Concern[TileTester, InjectingAspect[TileTester, _]] {
  override def aspects: Seq[InjectingAspect[TileTester, _]] = {
     Seq(InjectingAspect({ tester: TileTester => tester.dut.asInstanceOf[mini.Tile].core.dpath.alu }, { alu: ALU =>
       withClockAndReset(alu.clock, alu.reset) {
         when(alu.io.alu_op === 0.U) {
           printf("A == %d, B == %d, opcode == %d\n", alu.io.A, alu.io.B, alu.io.alu_op)
         }
       }
     }))
  }
}

class TileSimpleTests extends TileTests(SimpleTests, concerns = Seq(MyMiniTestLogger))
