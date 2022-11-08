package mini
package cache

import chisel3._
import chisel3.util._
import foam._
import foam.aspects._

//is this best done as a trait?
trait HasMiddleUpdate extends HasMiddleAllocate with HasSimpleWrite {
  val updateCond = fsmHandle("sWriteCache") && hit && !cpu.abort
  
  middle.update(data, mask, updateCond)
}
