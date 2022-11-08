package mini
package cache

import chisel3._
import foam._
import foam.aspects._

trait HasCleanRead extends Cache {
  readDone := back.read((address(p.xlen - 1, p.offsetLen) << p.offsetLen.U).asUInt, buffer, hit, dirtyRead, isRead)
}