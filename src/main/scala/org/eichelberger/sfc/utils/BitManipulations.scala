package org.eichelberger.sfc.utils

object BitManipulations {
  // if the first on-bit is at position P, then this routine returns a
  // mask in which all of the bits from 0..P are turned on, and all of
  // the bits from P+1..63 are off
  def usedMask(x: Long): Long = {
    var y = x | (x >> 1L)
    y = y | (y >> 2L)
    y = y | (y >> 4L)
    y = y | (y >> 8L)
    y = y | (y >> 16L)
    y | (y >> 32L)
  }

  def sharedBitPrefix(a: Long, b: Long): Long =
    a & ~usedMask(a ^ b)

  def commonBlockMin(a: Long, b: Long): Long =
    a & ~usedMask(a ^ b)

  def commonBlockMax(a: Long, b: Long): Long = {
    val mask = usedMask(a ^ b)
    (a & ~mask) | mask
  }

}
