package myminiTests

import chisel3.aop._
import chisel3._
import chisel3.experimental.BaseModule
import aoplib.redundancy._
import mini._

object MyRedundancyAspects {

  def selectDpath(tester: TileTester): Datapath =
    tester.dut.asInstanceOf[mini.Tile].core.dpath

  def selectCaches(tester: TileTester): Iterable[Cache] = {
    tester.collectDeep {
      case c: Cache => c
    }
  }
  def selectMultiWayCaches(tester: TileTester): Iterable[Cache] = {
    tester.collectDeep {
      case c: Cache if c.p(NWays) >= 2 => c
    }
  }
  def selectRegisters(tester: TileTester): Iterable[Data] = tester.getDeep { _.registers() }
  def selectMemories(tester: TileTester): Iterable[Mem[_]] = tester.getDeep { _.mems() }
  def selectMuxes(tester: TileTester): Iterable[Data] = tester.getDeep { _.ops("mux") }


  val redundantRegs = RedundancyAspect(
    { dut: TileTester => selectDpath(dut).getDeep { _.registers() } }
  )

  val redundantInst = RedundancyAspect(
    { dut: TileTester => Seq(selectDpath(dut).ew_inst) }
  )

  val faultyInst = StuckFaultAspect(
    { dut: TileTester => Seq(selectDpath(dut).ew_inst) }
  )

  val faultyCache = StuckFaultAspect(
    {dut: TileTester => selectCaches(dut).map(_.addr_reg).toList }
  )

}

class TileSimpleTestsWithRedundancy extends TileTests(SimpleTests, aspects = Seq(MyRedundancyAspects.redundantInst))


class TileSimpleTestsWithRedundancyAndFault extends TileTests(SimpleTests, aspects = Seq(MyRedundancyAspects.redundantInst, MyRedundancyAspects.faultyInst))

// This will fail because it injects a fault
class TileSimpleTestsWithFault extends TileTests(SimpleTests, aspects = Seq(MyRedundancyAspects.faultyInst))

// This will pass even when it injects a fault because we also add triple modular redundancy to every datapath register
class TileSimpleTestsWithRegRedundancyAndFault extends TileTests(SimpleTests,
  aspects = Seq(MyRedundancyAspects.redundantRegs, MyRedundancyAspects.faultyInst)
)

class TileSimpleTestsWithFaultyCaches extends TileTests(SimpleTests, aspects = Seq(MyRedundancyAspects.faultyCache))
