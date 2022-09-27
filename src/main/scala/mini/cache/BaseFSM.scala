package mini
package cache

import foam._

object BaseFSM {
  def apply() = {
    //create the base finite state machine
    val sIdle = new IdleState("sIdle")
    val sReadCache = new ReadState("sReadCache")
    val sRefill = new MemoryState("sRefill")

    val readReq = CacheToken("readReq")
    val readFinish = CacheToken("readFinish")
    val cleanMiss = CacheToken("cleanMiss")
    val refillFinish = CacheToken("refillFinish")

    (new NFA(sIdle))
    .addTransition((sIdle, readReq), sReadCache)
    .addTransition((sReadCache, readFinish), sIdle)
    .addTransition((sReadCache, cleanMiss), sRefill)
    .addTransition((sRefill, refillFinish), sIdle)
  }
}