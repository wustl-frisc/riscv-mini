package mini

import foam._

case class CacheState(code: () => Unit) extends ChiselState {
  override val isAccept = true
}

object CacheStateFactory {
  def apply(body: => Any) = CacheState(() => {
    body
  })
}