package mini
package aspects

import faust._
import chisel3._
import chisel3.util._

class PlaceCounters(classLoc: String, func: String, ret: String = "Any") extends Aspect {
  After((classLoc, func), ret) {() => {
    import mini.counter._
    val perfCounters = PerformanceCounters(Set[EventSet](mini.cache.CacheEvents), 4)
  }}
}