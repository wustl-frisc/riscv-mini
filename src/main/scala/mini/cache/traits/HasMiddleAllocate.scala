package mini
package cache

import chisel3._
import chisel3.util._
import foam._
import foam.aspects._

trait HasMiddleAllocate extends Cache {
  val middle = new Middleend(fsmHandle, p, address, tag, index, valids)
  val (oldTag, readData) = middle.read(buffer, nextAddress, offset, Some(hit), Some(cpu))
  middle.allocate(Cat(mainMem.r.bits.data, Cat(buffer.init.reverse)), readDone)
}
