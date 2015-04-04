package org.eichelberger.sfc

object GenericTesting {
  val bboxCville = (-78.5238, 38.0097, -78.4464, 38.0705)

  def time[T](a: () => T): (T, Long) = {
    val nanoStart = System.nanoTime()
    val result = a()
    val nanoStop = System.nanoTime()
    val nanosElapsed = nanoStop - nanoStart
    val msElapsed = nanosElapsed / 1e6.toLong
    (result, msElapsed)
  }
}