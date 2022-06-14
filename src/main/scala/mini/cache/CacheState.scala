package mini

import foam._

case class CacheState(override val code: Any) extends ChiselState {
  override val isAccept = true
  override def toString = super.toString + code.toString
}