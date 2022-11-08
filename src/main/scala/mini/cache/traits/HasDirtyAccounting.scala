package mini
package cache

import chisel3._
import foam._
import foam.aspects._

trait HasDirtyAccounting extends Cache {
  val readJustDone = RegNext(readDone)
  val dirty = RegInit(0.U(p.nSets.W))
}
