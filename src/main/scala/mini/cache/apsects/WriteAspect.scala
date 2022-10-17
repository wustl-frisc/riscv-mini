package mini
package cache

import foam._
import foam.aspects._

class WriteAspect extends Aspect[NFA] {
  val writeCache = WriteState("sWriteCache")
  val writeSetup = WriteSetupState("sWriteSetup")
  val writeWait = WriteWaitState("sWriteWait")

  val writeReq = CacheToken("writeReq")
  val writeFinish = CacheToken("writeFinish")
  val writeMiss = CacheToken("writeMiss")
  val memWait = CacheToken("memWait")
  val ack = CacheToken("ack")

  def apply(nfa: NFA) = {

    val idlePointcut = Pointcutter[State, IdleState](
      nfa.states,
      state =>
        state match {
          case s: IdleState => true
          case _ => false
        }
    )

    AroundState[IdleState](idlePointcut, nfa)((thisJoinpoint: Joinpoint[IdleState], thisNFA: NFA) => {
      val newNFA = thisNFA
        .addTransition((thisJoinpoint.point, writeReq), writeCache)
        .addTransition((writeCache, writeFinish), thisJoinpoint.point)
        .addTransition((writeCache, writeMiss), writeSetup)
        .addTransition((writeSetup, memWait), writeWait)
      (thisJoinpoint.point, newNFA)
    })
  }
}
