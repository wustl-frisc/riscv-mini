package mini
package aspects

import faust._

class Passthrough (pointcut: (String, String)) extends Aspect {
  Extend(pointcut)("HasTinyConfig")
  Extend(pointcut)("HasBufferBookeeping")
}
