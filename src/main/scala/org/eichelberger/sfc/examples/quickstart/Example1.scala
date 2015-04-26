package org.eichelberger.sfc.examples.quickstart

object Example1 extends App {
  import org.eichelberger.sfc.examples.Geohash

  // create the curve
  val totalPrecision = 35  // bits to be allocated between longitude and latitude
  val geohash = new Geohash(totalPrecision)

  // compute a hash from a (lon, lat) point
  val hashCville = geohash.pointToHash(Seq(-78.49, 38.04))
  println(s"Representative Charlottesville point hashes to $hashCville")

  // compute the (inverse) cell from the hash
  val cellCville = geohash.hashToCell(hashCville)
  println(s"Representative Charlottesville hash corresponds to cell $cellCville")
}
