package org.eichelberger.sfc.utils

object Timing {
  def time[T](a: () => T): (T, Long) = {
    val nanoStart = System.nanoTime()
    val result = a()
    val nanoStop = System.nanoTime()
    val nanosElapsed = nanoStop - nanoStart
    val msElapsed = nanosElapsed / 1e6.toLong
    (result, msElapsed)
  }
}
