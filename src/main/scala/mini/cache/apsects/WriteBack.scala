package mini
package cache

import foam._
import foam.aspects._

class WriteBack extends Aspect[NFA] {
  private val ack = CacheToken("ack")

  override def apply(nfa: NFA) = {

    val waitPointcut = Pointcutter[State, WriteWaitState](
      nfa.states,
      state =>
        state match {
          case s: WriteWaitState => true
          case _ => false
        }
    )

    AfterState[WriteWaitState](waitPointcut, nfa)((thisJoinpoint: StateJoinpoint[WriteWaitState], thisNFA: NFA) => {
      thisJoinpoint.out match {
        case Some(t) => (None, thisNFA)
        case _       => (Some(ack, ReadState("sReadCache")), thisNFA)
      }
    })
  }
}
