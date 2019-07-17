package aoplib.coverage

import chisel3.Data

//trait CoverBase { val label: String }

case class CoverPoint(label: String, signal: Data, bins: Seq[BaseBin], pointOptions: CoverOptions) //extends CoverBase
case class CoverOptions(weight: Int = 1)

// TODO: I think you can get away without representing this directly and generating it programmatically
// case class CrossPoint(name: String, points: Seq[CoverPoint], bins: Seq[BaseBin]) extends CoverBase

abstract class BaseBin {
  val labelOption: Option[String]
  val category: BinCategory
}

// Explicit bin, bins based on category
case class Bin(label: String, category: BinCategory) extends BaseBin {
  override val labelOption: Option[String] = Some(label)
}

// Implicit bin, bins based on category
// Not user created
case class ImplicitBin(category: BinCategory) extends BaseBin {
  override val labelOption: Option[String] = None
}

// Ignores when bin matches (usually paired with ImplicitBin
case class IgnoreBin(label: String, category: BinCategory) extends BaseBin {
  override val labelOption: Option[String] = Some(label)
}

// Kills simulation if bin matches
case class InvalidBin(label: String, category: BinCategory) extends BaseBin {
  override val labelOption: Option[String] = Some(label)
}


trait BinCategory

// Defaults to all non-specified categories
case object Default extends BinCategory

// Low and High are inclusive
case class BinRange(low: BigInt, high: BigInt) extends BinCategory

// A sequence of values that must be transitioned to, in order
case class BinTransition(sequence: Seq[BinValue]) extends BinCategory

trait BinValue

// A value in a sequence that must match immediately
case class BinConstant(value: BigInt) extends BinValue

// A value that must be hit eventually, but not necessarily at this time
case class BinEventually(value: BigInt) extends BinValue


