package mini
package cache

import foam._

object ReadFSM {
  val sIdle = new IdleState("sIdle")
  val sReadCache = new ReadState("sReadCache")
  val sRefill = new RefillState("sRefill")

  val readReq = CacheToken("readReq")
  val readFinish = CacheToken("readFinish")
  val readMiss = CacheToken("readMiss")
  val refillFinish = CacheToken("refillFinish")

  def apply() = {
    //create the base finite state machine

    (new NFA(sIdle))
      .addTransition((sIdle, readReq), sReadCache)
      .addTransition((sReadCache, readFinish), sIdle)
      .addTransition((sReadCache, readMiss), sRefill)
      .addTransition((sRefill, refillFinish), sIdle)
  }
}
