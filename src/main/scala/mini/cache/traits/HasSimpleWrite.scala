package mini
package cache

import chisel3._
import foam._
import foam.aspects._

trait HasSimpleWrite extends Cache {
  val (data, mask) = front.write(offset)
  back.write((address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt, data, Some(mask), offset, cpu.abort)
}
