package aoplib.histogram

import chisel3.experimental.MultiIOModule
import chisel3._

/** Either records and bins the value of [in] every cycle, or cycles through all bins and prints the result
  *
  * @param hinfo contains signal (and its type) as well as other histogramming information
  * @param name name of the instance of the parent module containing the signal
  * @param module name of the parent module containing the signal
  */
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
