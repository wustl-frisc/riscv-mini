package mini
package cache

import chisel3._
import foam._
import foam.aspects._

trait HasInvalidOnWrite extends Cache {
  when(fsmHandle("sWriteWait")) { //IvdLine
      valids := valids.bitSet(index, false.B)
  }
}