package myminiTests

//import aoplib.{HistogramAspect, LoggingAspect, LoggingInfo}
import aoplib.{HistogramAspect, HistogramSignal}
import chisel3._
import chisel3.aop.{Aspect, Concern, InjectingAspect, InjectingConcern}
import chisel3.core.{RunFirrtlTransform, annotate, dontTouch}
import chisel3.experimental.RawModule
import chisel3.util.experimental.BoringUtils
import firrtl.annotations.NoTargetAnnotation
import firrtl.passes.wiring.WiringTransform
import freechips.rocketchip.config.Parameters
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

case object MyMiniTestLogger extends InjectingConcern[TileTester, InjectingAspect[TileTester, _]] {
  class Logger(p: Parameters) extends Module {
    val io = IO(new Bundle {
      val A = Input(UInt())
      val B = Input(UInt())
      val alu_op = Input(UInt(4.W))
    })
    when(io.alu_op === 0.U) {
      printf("A == %d, B == %d, opcode == %d\n", io.A, io.B, io.alu_op)
    }
  }
  override def aspects: Seq[InjectingAspect[TileTester, _]] = {
    val inlined = InjectingAspect(
      { tester: TileTester => tester.dut.asInstanceOf[mini.Tile].core.dpath.alu },
      { alu: ALU =>
        when(alu.io.alu_op =/= 0.U) {
          printf("A == %d, B == %d, opcode == %d\n", alu.io.A, alu.io.B, alu.io.alu_op)
        }
      }
    )
    val connected = InjectingAspect(
      { tester: TileTester => tester.dut.asInstanceOf[mini.Tile].core.dpath.alu },
      { alu: ALU =>
        //alu.io.out := 0.U
      }
    )
    val instanced = InjectingAspect(
      { tester: TileTester => tester.dut.asInstanceOf[mini.Tile].core.dpath.alu },
      { alu: ALU =>
        val logger = Module(new Logger(alu.p))
        logger.io.A := alu.io.A
        logger.io.B := alu.io.B
        logger.io.alu_op := alu.io.alu_op
      }
    )
    val annotated = InjectingAspect(
      { tester: TileTester => tester.dut.asInstanceOf[mini.Tile].core.dpath.alu },
      { alu: ALU =>
        dontTouch(alu.io.B)
      }
    )
    Seq(inlined)
    //Seq(connected)
    //Seq(instanced)
    //Seq(annotated)
    //Seq(inlined, connected, instanced)
  }
}

case class RunBoring() extends Concern[TileTester, Aspect[TileTester, _]] {
  override def aspects = Nil
  def transformClass = classOf[WiringTransform]
}

case object MyMiniTestHistogram extends InjectingConcern[TileTester, HistogramAspect[TileTester, _]] {
  override def aspects: Seq[HistogramAspect[TileTester, _]] = {
    val aluHistogram = HistogramAspect(
      { tester: TileTester => tester.dut.asInstanceOf[mini.Tile].core.dpath.alu },
      { alu: ALU =>
        Seq(
          new HistogramSignal(alu.io.alu_op),
          new HistogramSignal(alu.io.A) {
            override def untilMax: Int = 1000
            override def nBins: Int = 10
          },
        )
      },
      { tester: TileTester =>
        tester.isDone
      },
      { tester: TileTester =>
        tester.setDone
      }
    )
    Seq(aluHistogram)
  }
}

class TileSimpleTests extends TileTests(SimpleTests, concerns = Seq(MyMiniTestHistogram, RunBoring()/*, MyMiniTestLogger*/))

