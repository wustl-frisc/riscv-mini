package mini
package cache

import chisel3._
import chisel3.util._
import foam._
import foam.aspects._

trait HasSimpleWrite extends Cache {
  val (data, mask) = front.write(offset, writeDone)

  val writeAddress = (address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt
  val writeData = data
  val writeMask = Some(mask)
  
  back.write(writeAddress, writeData, writeMask, offset, localWrite, dirtyWrite)
}
