package org.eichelberger.sfc

object GenericTesting {
  val bboxCville = (-78.5238, 38.0097, -78.4464, 38.0705)

  def time[T](a: () => T): (T, Long) = {
    val msStart = System.currentTimeMillis()
    val result = a()
    val msStop = System.currentTimeMillis()
    (result, msStop - msStart)
  }
}