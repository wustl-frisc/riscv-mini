package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import junctions._

object TileState extends ChiselEnum {
  val sIdle, sWrite, sWrAck, sRead = Value
}

object TestUtils {
  def resizeHexFile(originalHexFile: os.Path, resizedHexFile: os.Path, resizedWidth: Int): Unit = {
    val hexFileLines = os.read.lines(originalHexFile)
    val resizedLines = resizeHexFileInner(hexFileLines, resizedWidth)
    os.write.over(resizedHexFile, resizedLines.mkString("\n"), createFolders = true)
  }

  def resizeHexFileInner(hexFileLines: Seq[String], resizedWidth: Int): Seq[String] = {
    val hexFileLineLengths = hexFileLines.filter(_.nonEmpty).map(_.length)
    assert(hexFileLineLengths.forall(_ == hexFileLineLengths.head), "hex file has lines of differing lengths")
    val originalWidth = hexFileLineLengths.head * 4 // 4 bits / nibble

    if (originalWidth > resizedWidth) {
      assert(
        originalWidth % resizedWidth == 0,
        f"The original width of the hex file ($originalWidth) is not evenly divisible by the desired resized width ($resizedWidth)"
      )

      hexFileLines.flatMap(_.grouped(resizedWidth / 4).toSeq.reverse)
    } else if (originalWidth < resizedWidth) {
      ???
    } else { // no resizing
      hexFileLines
    }
  }
}

class TestHarness (tile: => Tile) extends Module {
  val benchmark = "rv32ui-p-simple"
  val originalHexFile = os.rel / "tests" / f"$benchmark.hex"
  val resizedHexFile = os.rel / "tests" / "64" / f"$benchmark.hex"
  TestUtils.resizeHexFile(os.pwd / originalHexFile, os.pwd / resizedHexFile, 64) // we have 64 bits per memory entry

  val led = IO(Output(Bool()))
  led := false.B

  val dut = Module(tile)
  // extract parameters from design under test
  val nasti = dut.nastiParams

  dut.io.host.fromhost.bits := 0.U
  dut.io.host.fromhost.valid := false.B

  val _mem = Mem(1 << 8, UInt(nasti.dataBits.W))
  loadMemoryFromFileInline(_mem, resizedHexFile.toString())
  import TileState._
  val state = RegInit(sIdle)

  val id = Reg(UInt(nasti.idBits.W))
  val addr = Reg(UInt(nasti.addrBits.W))
  val len = Reg(UInt(NastiConstants.LenBits.W))
  val off = Reg(UInt(NastiConstants.LenBits.W))

  val blastBytes = nasti.dataBits / 8
  val write = (VecInit.tabulate(blastBytes)(byte => (Mux(dut.io.nasti.w.bits.strb(byte), dut.io.nasti.w.bits.data, _mem(addr + off)))(8 * (byte + 1) - 1, 8 * byte))).asUInt

  val bpipe = WireInit(dut.io.nasti.b)
  val rpipe = WireInit(dut.io.nasti.r)

  dut.reset := reset.asBool
  dut.io.nasti.aw.ready := state === sIdle
  dut.io.nasti.ar.ready := state === sIdle
  dut.io.nasti.w.ready := state === sWrite
  dut.io.nasti.b <> bpipe
  dut.io.nasti.r <> rpipe
  bpipe.bits := NastiWriteResponseBundle(nasti)(id)
  bpipe.valid := state === sWrAck
  rpipe.bits := NastiReadDataBundle(nasti)(id, _mem(addr + off), off === len)
  rpipe.valid := state === sRead

  val isDone = WireInit(false.B)
  val setDone = WireInit(false.B)

  when(dut.io.host.tohost =/= 0.U) {
    led := true.B
    dut.reset := true.B
  }

  /**
    *   Master(Tile)    ---------   Slaver(Memory)
    *
    *   Address Write > == AW == >
    *   Data Write    > == W  == >
    *   Response      < == B  == <
    *
    *   Address Read  > == AR == >
    *   Data Read     < == R  == <
    */

  switch(state) {
    is(sIdle) {
      when(dut.io.nasti.aw.valid) {
        addr := dut.io.nasti.aw.bits.addr / (nasti.dataBits / 8).U
        id := dut.io.nasti.aw.bits.id
        len := dut.io.nasti.aw.bits.len
        off := 0.U
        state := sWrite
      }.elsewhen(dut.io.nasti.ar.valid) {
        addr := dut.io.nasti.ar.bits.addr / (nasti.dataBits / 8).U
        id := dut.io.nasti.ar.bits.id
        len := dut.io.nasti.ar.bits.len
        off := 0.U
        state := sRead
      }
    }
    is(sWrite) {
      when(dut.io.nasti.w.valid) {
        _mem(addr + off) := write
        when(off === len) {
          state := sWrAck
        }.otherwise {
          off := off + 1.U
        }
      }
    }
    is(sWrAck) {
      when(bpipe.ready) {
        state := sIdle
      }
    }
    is(sRead) {
      when(rpipe.ready) {
        when(off === len) {
          state := sIdle
        }.otherwise {
          off := off + 1.U
        }
      }
    }
  }
}
