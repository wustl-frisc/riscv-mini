package mini
package cache

import foam._
import foam.aspects._

class WriteBypass extends Aspect[NFA] {
  private val writeCache = WriteState("sWriteCache")
  private val writeSetup = WriteSetupState("sWriteSetup")
  private val writeWait = WriteWaitState("sWriteWait")
  
  private val writeReq = CacheToken("writeReq")
  private val writeFinish = CacheToken("writeFinish")
  private val writeMiss = CacheToken("writeMiss")
  private val memWait = CacheToken("memWait")
  private val ack = CacheToken("ack")

  def apply(nfa: NFA) = {

    val idlePointcut = Pointcutter[State, IdleState](nfa.states, state => state match {
      case s: IdleState => true
      case _ => false
    })

    val step1 = AroundState[IdleState](idlePointcut, nfa)((thisJoinpoint: Joinpoint[IdleState], thisNFA: NFA) => {
      val newNFA = thisNFA.addTransition((thisJoinpoint.point, writeReq), writeCache)
        .addTransition((writeCache, writeFinish), thisJoinpoint.point)
        .addTransition((writeCache, writeMiss), writeSetup)
        .addTransition((writeSetup, memWait), writeWait)
      (thisJoinpoint.point, newNFA)
    })

    val waitPointcut = Pointcutter[State, WriteWaitState](step1.states, state => state match {
      case s: WriteWaitState => true
      case _ => false
    })

    AfterState[WriteWaitState](waitPointcut, step1)((thisJoinpoint: StateJoinpoint[WriteWaitState], thisNFA: NFA) => {
      thisJoinpoint.out match {
        case Some(t) => (None, thisNFA)
        case _ => (Some(ack, IdleState("sIdle")), thisNFA)
      }
    })
  }
}