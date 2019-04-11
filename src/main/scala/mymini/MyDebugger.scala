package mymini

import aoplib.HistogramAspect
import chisel3.Data
import firrtl.annotations.ReferenceTarget
import mini.instrumentation.CSRDebugger.CSRHistogram

case object MyMiniHistogram extends HistogramAspect[mini.Tile] {
  override def histogramSignals(dut: mini.Tile): Seq[Data] = {
    val named = dut.core.dpath.csr.csrValid
    Seq(named) ++ CSRHistogram.histogramSignals(dut.core.dpath.csr)
  }
}

