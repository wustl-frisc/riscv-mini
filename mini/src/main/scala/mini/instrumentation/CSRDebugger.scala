package mini.instrumentation

import aoplib.{HistogramAspect, LoggingAspect, LoggingInfo}
import chisel3.Data

object CSRDebugger {
  case object CSRHistogram extends HistogramAspect[mini.CSR] {
    override def histogramSignals(csr: mini.CSR): Seq[Data] = {
      Seq(csr.PRV, csr.csr_addr)
    }
  }
}

case object CSRLogger extends LoggingAspect[mini.CSR] {
  override def logSignals(tester: mini.CSR): Seq[LoggingInfo] = {
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

