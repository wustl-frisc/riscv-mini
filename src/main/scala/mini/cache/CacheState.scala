package mini
package cache

import foam._

abstract class CacheState(override val id: String) extends State {
  override val isAccept = true
}

case class IdleState(override val id: String) extends CacheState(id)

case class ReadState(override val id: String) extends CacheState(id)

case class WriteSetupState(override val id: String) extends CacheState(id)

case class WriteWaitState(override val id: String) extends CacheState(id)

case class WriteState(override val id: String) extends CacheState(id)

case class MemoryState(override val id: String) extends CacheState(id)