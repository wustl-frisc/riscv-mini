package mymini

import aoplib.floorplan.FloorplanAspect
import chisel3.{Data, Vec}
import chisel3.Bundle
import floorplan._
import mini.TileTester
import mini._
import FloatingPointDimensionImplicits._
import chisel3.aop.Select
import firrtl.options.{RegisteredLibrary, ShellOption}

object Floorplans {

  def layoutCache(cache: Cache, name: String): LayoutBase = {
    val elements = cache.dataMem.foldLeft(Seq[LayoutBase](VerticalExpander())){
      (elems, m) => elems :+ HardMacro(m.toTarget.serialize, m.length.toInt, m.t.getWidth) :+ VerticalExpander()
    }
    VBox(cache, name, elements)
  }


  def layoutPort(port: Data): LayoutBase = {
    HardMacro(port, port.toTarget.serialize, 2, 2)
  }

  def layoutCore(core: Core): LayoutBase = {
    val corePorts = Select.ios(core).flatMap(Select.getLeafs)
    HBox(core, "Core", Seq(
      HorizontalExpander(),
      VBox("Core Ports", corePorts.map(layoutPort) :+ VerticalExpander())
    ))
  }

  def layoutTester(tester: TileTester, tileLayout: LayoutBase): LayoutBase = {
    val testerLayout = {
      val testerPorts = Select.ios(tester).flatMap(Select.getLeafs)
      val testerPortLayout = HBox("Ports", HorizontalExpander() +: testerPorts.map(layoutPort))
      VBox(tester, "Top", Seq(tileLayout, VerticalExpander(), testerPortLayout))
    }

    testerLayout.replaceWidthAndHeight(500, 800)
  }

  def layoutTile(btile: TileBase, option: Int): LayoutBase = {
    val tile = btile.asInstanceOf[mini.Tile]
    val coreLayout = layoutCore(tile.core)

    val dcacheLayout = layoutCache(tile.dcache, "D$")

    val icacheLayout = VBox("ICache", Seq(
      layoutCache(tile.icache, "I$").replaceHeight(300),
      VerticalExpander())
    )

    val tileLayout = HBox(tile, "Tile", option match {
      case 0 => Seq(dcacheLayout, coreLayout, icacheLayout)
      case 1 => Seq(icacheLayout, coreLayout, dcacheLayout)
    }).replaceWidthAndHeight(500, 500)

    tileLayout
  }

  val floorplan = FloorplanAspect(
    "MyFloorplan",
    "test_run_dir/html/myfloorplan",
    (tester: TileTester) => { layoutTester(tester, layoutTile(tester.dut, 0)) }
  )

  val floorplan2 = FloorplanAspect(
    "MyFloorplan",
    "test_run_dir/html/myfloorplan",
    (tester: TileTester) => { layoutTester(tester, layoutTile(tester.dut, 1)) }
  )

}

case class MiniFloorplan() extends RegisteredLibrary {
  val name = "Mini-Floorplan"
  val options = Seq(new ShellOption[String](
    longOption = "floorplan",
    toAnnotationSeq = {
      case "dci" => Seq(FloorplanAspect("Mini_DCI","test_run_dir/html/myfloorplan",{ t: TileBase => Floorplans.layoutTile(t, 0) }))
      case "icd" => Seq(FloorplanAspect("Mini_ICD","test_run_dir/html/myfloorplan",{ t: TileBase => Floorplans.layoutTile(t, 1) }))
    },
    helpText = "The name of a mini floorplan must be <dci|icd> indicating the relative positions of the icache, core, and dcache.",
    helpValueName = Some("<dci|icd>")))
}
