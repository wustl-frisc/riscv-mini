package mini
package counter

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

case class Event(id: UInt, name: String)

abstract class EventSet(val id: UInt) {
  val events = scala.collection.mutable.Set[Event]()

  def apply(id: UInt, name: String, signal: Bool) = {
    BoringUtils.addSource(signal, name)
    events += Event(id, name)
  }
}

object PerformanceCounters {

  def apply(eventSets: Set[EventSet], numCounters: Int): Array[(UInt, mini.counter.Counter)] = {
    val signalArray = VecInit.fill(eventSets.size, 32)(Reg(Bool()))
    
    eventSets.foreach(es => { //for each event set
      es.events.foreach(e => { //for each event
        val eventSignal = WireInit(false.B)
        BoringUtils.addSink(eventSignal, e.name) //sink to the id bit
        signalArray(es.id)(e.id) := eventSignal
      })
    })

    //build out the counters
    Array.fill(numCounters) {
      val configReg = RegInit(0.U(64.W))
      val counter = new mini.counter.Counter() with OnRisingEdge

      //connect to the correct event set
      val eventVec = signalArray(configReg(63, 32)) //use the upper 32 bits for event set
      //count only when all configured events have triggered
      counter.count((eventVec.asUInt & configReg(31, 0)) === configReg(31, 0)) //use lower 32 bits for individual events
      (configReg, counter)
    }
  }
}
