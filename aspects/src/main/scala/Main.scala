package mini
package aspects

import faust._

object Cache0Cache1 extends App {
  Weaver {
    new Passthrough(("ICache", "*"))
    new Passthrough(("DCache", "*"))
    new CacheInvalidOnWrite
    //new SimpleCounter
    //new PlaceCounters("Tile", "hookUp")
    //new PlaceCounters("CSR", "counterBlock")
    //new AddCSR
  }
}

object UnWeave extends App {
  Weaver.unWeave()
}
