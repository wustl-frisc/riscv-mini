package mini

import foam._
import chisel3._

case class CacheToken(override val cond: Bool) extends ChiselToken