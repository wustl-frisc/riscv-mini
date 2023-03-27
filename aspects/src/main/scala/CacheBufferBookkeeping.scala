package mini
package aspects

import faust._

class CacheBufferBookkeeping (name: String) extends Aspect {
  Extend(("Cache", name))("HasBufferBookeeping")
}
