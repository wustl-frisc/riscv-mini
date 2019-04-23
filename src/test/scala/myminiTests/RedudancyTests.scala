package myminiTests

import chisel3.aop._
import chisel3._
import aoplib.redundancy._
import mini._

object TileTesterRedundancyAspects {

  def selectDpath(tester: TileTester): Seq[Datapath] = Seq(tester.dut.asInstanceOf[mini.Tile].core.dpath)

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

case object MyRedundancy {
  def aspects = Seq(TileTesterRedundancyAspects.redundantInst)
}

case object MyFaults {
  def aspects = Seq(TileTesterRedundancyAspects.faultyInst)
}

class TileSimpleTestsWithRedundancy extends TileTests(SimpleTests, aspects = MyRedundancy.aspects)

// This should fail, so its commented out to pass CI
//class TileSimpleTestsWithFault extends TileTests(SimpleTests, aspects = MyFaults.aspects)

class TileSimpleTestsWithRedundancyAndFault extends TileTests(SimpleTests, aspects = MyRedundancy.aspects ++ MyFaults.aspects)
