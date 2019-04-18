package myminiTests

import chisel3.aop._
import chisel3._
import aoplib.redundancy._
import mini._

object TileTesterRedundancyAspects {

  def selectDpath(tester: TileTester): Datapath = tester.dut.asInstanceOf[mini.Tile].core.dpath

  val redundantInst = RedundancyAspect(
    selectDpath,
    {dpath: Datapath =>
      Seq(dpath.ew_inst)
    }
  )

  val faultyInst = StuckFaultAspect(
    selectDpath,
    {dpath: Datapath =>
      Seq(dpath.ew_inst)
    }
  )
}

case object MyRedundancy extends RedundancyConcern[TileTester, RedundancyAspect[TileTester, _]] {
  def aspects = Seq(TileTesterRedundancyAspects.redundantInst)
}

case object MyFaults extends StuckFaultConcern[TileTester, StuckFaultAspect[TileTester, _]] {
  def aspects = Seq(TileTesterRedundancyAspects.faultyInst)
}

class TileSimpleTestsWithRedundancy extends TileTests(SimpleTests, concerns = Seq[Concern[TileTester, _]](MyRedundancy))
class TileSimpleTestsWithFault extends TileTests(SimpleTests, concerns = Seq[Concern[TileTester, _]](MyFaults))
class TileSimpleTestsWithRedundancyAndFault extends TileTests(SimpleTests, concerns = Seq[Concern[TileTester, _]](MyRedundancy, MyFaults))
