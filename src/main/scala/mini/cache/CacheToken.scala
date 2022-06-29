package mini

import foam._
import chisel3._

case class CacheToken(override val cond: Bool, override val id: String) extends ChiselToken