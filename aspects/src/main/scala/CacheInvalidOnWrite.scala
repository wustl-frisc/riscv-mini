package mini
package aspects

import faust._

class CacheInvalidOnWrite extends Aspect {
  Extend(("DCache", "dcache"))("HasInvalidOnWrite")
}
