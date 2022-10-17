package mini
package cache

import foam._
import foam.aspects._

class DirtyAccounting extends Aspect[NFA] {

  val cleanMiss = CacheToken("cleanMiss")
  val dirtyMiss = CacheToken("dirtyMiss")
  val doWrite = CacheToken("doWrite")

  override def apply(nfa: NFA) = {

    val writePointcut = Pointcutter[State, WriteState](
      nfa.states,
      state =>
        state match {
          case s: WriteState => true
          case _ => false
        }
    )

    val step1 = AroundState[WriteState](writePointcut, nfa)((thisJoinpoint: StateJoinpoint[WriteState], thisNFA: NFA) => {
      val newNFA = thisNFA.addTransition((thisJoinpoint.point, cleanMiss), ReadFSM.sReadCache)
      (thisJoinpoint.point, newNFA)
    })

    val readPointcut = Pointcutter[State, ReadState](
      nfa.states,
      state =>
        state match {
          case s: ReadState => true
          case _ => false
        }
    )

    val step2 = AroundState[ReadState](readPointcut, step1)((thisJoinpoint: StateJoinpoint[ReadState], thisNFA: NFA) => {
      val newNFA = thisNFA.addTransition((thisJoinpoint.point, dirtyMiss), WriteFSM.sWriteCache)
      (thisJoinpoint.point, newNFA)
    })

    val refillPointcut = Pointcutter[State, RefillState](
      nfa.states,
      state =>
        state match {
          case s: RefillState => true
          case _ => false
        }
    )

    AroundState[RefillState](refillPointcut, step2)((thisJoinpoint: StateJoinpoint[RefillState], thisNFA: NFA) => {
      val newNFA = thisNFA.addTransition((thisJoinpoint.point, doWrite), WriteFSM.sWriteCache)
      (thisJoinpoint.point, newNFA)
    })
  }
}
