package mini
package cache

import chisel3._
import foam._
import foam.aspects._

trait HasWriteStub extends Cache {
  back.writeStub()
}
