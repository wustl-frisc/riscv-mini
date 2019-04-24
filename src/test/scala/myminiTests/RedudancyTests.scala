package myminiTests

import chisel3.aop._
import chisel3._
import chisel3.experimental.BaseModule
import aoplib.redundancy._
import mini._

object MyRedundancyAspects {

  def selectDpath(tester: TileTester): Seq[Datapath] = Seq(
    tester.dut.asInstanceOf[mini.Tile].core.dpath
  )

  def selectCaches(tester: TileTester): Seq[Cache] = {
    tester.getDeep { m: BaseModule => m.instances() }.collect {
      case c: Cache =>
        println(Seq(c.name, c.instanceName))
        c
    }
  }

  val redundantRegs = RedundancyAspect(
    selectDpath,
    { dpath: Datapath => dpath.registers() }
  )

  val redundantInst = RedundancyAspect(
    selectDpath,
    { dpath: Datapath => Seq(dpath.ew_inst) }
  )

  val faultyInst = StuckFaultAspect(
    selectDpath,
    {dpath: Datapath =>
      Seq(dpath.ew_inst)
    }
  )

  val faultyCache = StuckFaultAspect(
    selectCaches,
    {cache: Cache =>
      Seq(cache.addr_reg)
    }
  )
}

class TileSimpleTestsWithRedundancy extends TileTests(SimpleTests, aspects = Seq(MyRedundancyAspects.redundantInst))

// This should fail, so its commented out to pass CI
//class TileSimpleTestsWithFault extends TileTests(SimpleTests, aspects = Seq(MyRedundancyAspects.faultyInst))

class TileSimpleTestsWithRedundancyAndFault extends TileTests(SimpleTests, aspects = Seq(MyRedundancyAspects.redundantInst, MyRedundancyAspects.faultyInst))

class TileSimpleTestsWithRegRedundancyAndFault extends TileTests(SimpleTests, aspects = Seq(MyRedundancyAspects.redundantRegs, MyRedundancyAspects.faultyInst))

class TileSimpleTestsWithFaultyCaches extends TileTests(SimpleTests, aspects = Seq(MyRedundancyAspects.faultyCache))
