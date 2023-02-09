package mini.counter

import chisel3._

trait Split extends Counter{
  def getCountHighBits = counterReg >> 32
}
