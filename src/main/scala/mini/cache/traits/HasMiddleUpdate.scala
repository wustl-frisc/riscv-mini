package mini
package cache

import chisel3._
import foam._
import foam.aspects._

//is this best done as a trait?
trait HasMiddleUpdate extends HasMiddleAllocate with HasSimpleWrite {
  middle.update(data, mask, fsmHandle("sWriteCache") && hit && !cpu.abort)
}
