package mini
package cache

import foam._
import os.write

object WriteFSM {
  val sIdle = IdleState("sIdle")
  val sWriteCache = WriteState("sWriteCache")
  val sWriteSetup = WriteSetupState("sWriteSetup")
  val sWriteWait = WriteWaitState("sWriteWait")

  val writeReq = CacheToken("writeReq")
  val writeFinish = CacheToken("writeFinish")
  val writeMiss = CacheToken("writeMiss")
  val memWait = CacheToken("memWait")
  val ack = CacheToken("ack")

  def apply() = {
    //create the base finite state machine

    (new NFA(sIdle))
        .addTransition((sIdle, writeReq), sWriteCache)
        .addTransition((sWriteCache, writeMiss), sWriteSetup)
        .addTransition((sWriteSetup, memWait), sWriteWait)
        .addTransition((sWriteCache, writeFinish), sIdle)
  }
}
