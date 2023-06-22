package mini
package aspects

import faust._
import chisel3._
import chisel3.util._

class SimpleCounter extends Aspect {
  After(("Cache", "front.read"), "Any") { (hit: Bool) => {
      val event = if (this.isInstanceOf[mini.cache.ICache]) ("iCacheHit", 0.U) else ("dCacheHit", 1.U)
      mini.cache.CacheEvents(event._2, event._1, hit)
    }
  }
}