package aoplib

import chisel3._
import chisel3.aop._
import chisel3.experimental.{ChiselAnnotation, MultiIOModule, RawModule, annotate, dontTouch}
import chisel3.util.experimental.BoringUtils
import firrtl.annotations.{Annotation, ReferenceTarget, SingleTargetAnnotation}
import firrtl.{AnnotationSeq, CircuitForm, CircuitState, LowForm, RenameMap, ResolvedAnnotationPaths, Transform}

import scala.reflect.runtime.universe.TypeTag

abstract class HistogramConcern[T <: RawModule, R <: Aspect[T, _]](implicit tag: TypeTag[T]) extends Concern[T, R] {
  def aspects: Seq[R]
  override def transformClass: Class[_ <: Transform] = classOf[InjectingTransform]
}

class Histogram(hinfo: HistogramSignal, name: String, module: String) extends MultiIOModule {
  val in = IO(Input(chiselTypeOf(hinfo.signal)))
  val setHistogramming = IO(Input(Bool()))
  val setReadingOut = IO(Input(Bool()))
  val doneReading = IO(Output(Bool()))
  val allDone = IO(Input(Bool()))
  doneReading := false.B

  assert((setHistogramming === false.B) | (setReadingOut === false.B), "Inputs setHistogramming and setReadingOut cannot both be true")

  // Calculate in's address into histogram
  val ticks = chiselTypeOf(in) match {
    case s: SInt => sys.error("Haven't supported SInt's yet, sorry!!")
    case u: UInt =>
      val inWidth = in.getWidth
      val binInterval = (hinfo.untilMax - hinfo.minValue) / hinfo.nBins
      assert(binInterval * hinfo.nBins + hinfo.minValue == hinfo.untilMax,
        s"nBins ${hinfo.nBins} must divide evenly into the range from ${hinfo.minValue} until ${hinfo.untilMax}")
      val range = Range(hinfo.minValue, hinfo.untilMax + 1, binInterval)
      println(range.toList)
      VecInit(range.map(_.U))
  }
  val inU = in.asUInt()
  val (_, inAddr) = ticks.tail.zipWithIndex.foldLeft((ticks.head, 0.U)) { case ((min: UInt, addr: UInt), (max: UInt, index: Int)) =>
    (max, Mux((inU >= min) & (inU < max), index.U, addr))
  }

  // Instantiate histogram mem, actual read address (could change if not in histogramming mode), and readPort
  val histMem = Mem(math.pow(2, inAddr.getWidth).toInt, chiselTypeOf(hinfo.maxCount.U))
  val readAddress = Wire(chiselTypeOf(inAddr.asUInt))
  readAddress := 0.U
  val readPort = histMem.read(readAddress)

  // Calculate Read Value of input
  val readValue = Wire(chiselTypeOf(readPort))
  val hasWritten = RegInit(VecInit(Seq.fill(histMem.length)(false.B)))
  when(hasWritten(readAddress)) {
    readValue := readPort
  }.otherwise {
    readValue := 0.U
  }

  // Update histogram, or read out histogram
  // First, remember previous state of setReadingOut
  val pastSetReadingOut = RegNext(setReadingOut)

  // Then, do stuff
  when(reset.asBool() === false.B) {
    val readOutCounter = RegInit(chiselTypeOf(readAddress), 0.U)
    when(setHistogramming) {
      readAddress := inAddr.asUInt
      val writeValue = (readPort + 1.U).min(hinfo.maxCount.U)
      histMem.write(inAddr.asUInt(), writeValue)
      hasWritten(inAddr.asUInt()) := true.B
    }

    when(setReadingOut) {
      readAddress := readOutCounter
      when(pastSetReadingOut === false.B) {
        // First cycle we are reading out the histogram
        printf(s"Histogram for signal $name in module $module.\n")
      }
      val prevAddress = RegNext(readAddress)
      when(readAddress < (ticks.size - 1).U & (readAddress >= prevAddress)) {
        readOutCounter := readOutCounter + 1.U
        printf(s"Bin %d (%d until %d) -> %d\n", readAddress, ticks(readAddress), ticks(readAddress +& 1.U), readPort)
      }.otherwise {
        doneReading := true.B
      }
    }

    when(allDone) {
      doneReading := true.B
    }

  }
}

class HistogramSignal(val signal: Bits) {
  def maxCount = 100
  def minValue: Int = signal match {
    case _: UInt => 0
    case s: SInt => Math.pow(2, s.getWidth - 1).toInt
  }
  /* Until max is the smallest illegal value, or the max legal value plus 1 */
  def untilMax: Int = signal match {
    case u: UInt => Math.pow(2, u.getWidth).toInt
    case s: SInt => Math.pow(2, s.getWidth - 1).toInt
  }
  def nBins: Int = untilMax - minValue
}

case class HistogramAspect[DUT <: RawModule, M <: RawModule]
    (selectRoot: DUT => M, selectSignals: M => Seq[HistogramSignal], selectDesignDone: DUT => Bool, selectSimDone: DUT => Bool)
    (implicit tag: TypeTag[DUT]) extends Aspect[DUT, M](selectRoot) {


  def markDone(d: Data): Unit = {
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = firrtl.passes.wiring.SourceAnnotation(d.toTarget, "histogramDone")
    })
  }

  def setDone(d: Data): Unit = {
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = firrtl.passes.wiring.SinkAnnotation(d.toNamed, "histogramDone")
    })
  }

  def toTargets(dut: DUT): Seq[ReferenceTarget] = selectSignals(selectRoot(dut)).map(_.signal.toTarget)

  def toAnnotation(dut: DUT): AnnotationSeq = {
    val ia = InjectingAspect(selectRoot, { m: M =>
      val signals = selectSignals(m)
      val done = Wire(Bool())
      done := DontCare
      BoringUtils.bore(selectDesignDone(dut), Seq(done))
      val histograms = signals.map { s =>
        val histogram = Module(new Histogram(s, s.signal.name, m.name))
        histogram.in := s.signal
        histogram.setHistogramming := true.B
        histogram.setReadingOut := false.B
        histogram.allDone := false.B
        histogram
      }

      val allDone = WireInit(false.B)

      when(done) {
        histograms.foreach { h =>
          h.setHistogramming := false.B
        }
        allDone := histograms.foldLeft(1.U) { (readOut, h) =>
          h.setReadingOut := readOut
          val regNext = RegNext(h.doneReading)
          when(regNext) {
            h.setReadingOut := false.B
            h.allDone := true.B
          }

          readOut & h.doneReading
        }
      }

      dontTouch(allDone)
      markDone(allDone)
    }).toAnnotation(dut)

    val ia2 = InjectingAspect({dut: DUT => dut}, { dut: DUT =>
      setDone(selectSimDone(dut))
    }).toAnnotation(dut)

    ia ++ ia2
  }
}


case class SimulationDone(target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget): Annotation = SimulationDone(n)
}

/*
class HistogramTransform extends Transform with ResolvedAnnotationPaths {
  override def inputForm: CircuitForm = LowForm
  override def outputForm: CircuitForm = LowForm

  override def execute(state: CircuitState): CircuitState = {
    val doneSims = state.annotations.collect{ case x: SimulationDone => x.target }

    doneSims.foreach { case ReferenceTarget(_, mName, Nil, ref, Nil) =>


    }

  }
  import firrtl.ir._
  import firrtl.Mappers._

  def swapRef(ref: String)(d: DefModule): DefModule = d match {
    case m: Module => m map sw
  }

  def swapRefStmt(ref: String)(s: Statement): Statement = s map swapRefStmt(ref) match {

  }
}
*/
