package mini

import foam._

case class CacheState(override val id: String) extends State {
  override val isAccept = true
}