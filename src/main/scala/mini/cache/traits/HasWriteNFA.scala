package mini
package cache

import chisel3._
import foam._
import foam.aspects._

trait HasWriteNFA extends Cache {
  override lazy val cacheNFA = Weaver[NFA](List(new AckIdle), ReadFSM() + WriteFSM(), (before: NFA, after: NFA) => before.isEqual(after))
}