// See LICENSE for license details.

package mymini

import java.io.{File, FileWriter}

import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.TargetDirAnnotation
import mini.{MiniConfig, SimpleTests, Tile, TileTests}

object Main extends App {
  val params = (new MiniConfig).toInstance
  new chisel3.stage.ChiselStage().execute(args, Seq(
    ChiselGeneratorAnnotation(() => new Tile(params)),
    TargetDirAnnotation("test_run_dir")
  ))
}

object AspectMain extends App {
  //chisel3.Driver.emitVerilog()
}

class TileSimpleTests extends TileTests(SimpleTests)
