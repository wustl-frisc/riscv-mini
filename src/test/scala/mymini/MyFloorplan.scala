package mymini
import aoplib.floorplan.FloorplanAspect
import chisel3.{Data, Vec}
import chisel3.Bundle
import floorplan._
import fcl.Command._
import fcl._
import mini._
import FloatingPointDimensionImplicits._
import chisel3.aop.Select

import scala.io.Source

object MyFloorplan {
}

class TileSimpleTestsWithLayout extends TileTests(SimpleTests, annotations = Seq(mymini.Floorplans.floorplan), params = Some((new MiniConfig).toInstance alter { (site, here, up) => {
  case CacheBlockBytes => up(CacheBlockBytes)
  case Trace => false
}}))

class TileSimpleTestsWithLayoutWithLargerCache extends TileTests(SimpleTests, annotations = Seq(Floorplans.floorplan), params = Some((new MiniConfig).toInstance alter { (site, here, up) => {
  case CacheBlockBytes => up(CacheBlockBytes) * 2
  case Trace => false
}}))

class TileSimpleTestsWithBrokenLayout extends TileTests(SimpleTests, annotations = Seq(Floorplans.floorplan2), params = Some((new MiniConfig).toInstance alter { (site, here, up) => {
  case CacheBlockBytes => up(CacheBlockBytes) * 2
  case Trace => false
}}))


