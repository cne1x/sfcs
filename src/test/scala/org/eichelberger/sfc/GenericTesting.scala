package org.eichelberger.sfc

object GenericTesting {
  def time[T](a: () => T): (T, Long) = {
    val msStart = System.currentTimeMillis()
    val result = a()
    val msStop = System.currentTimeMillis()
    (result, msStop - msStart)
  }
}