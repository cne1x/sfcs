package org.eichelberger.sfc.examples.quickstart

object Example2 extends App {
  import org.eichelberger.sfc._
  import org.eichelberger.sfc.SpaceFillingCurve._

  // create a 4D curve
  val zCurve = new ZCurve(OrdinalVector(10, 20, 15, 4))

  // map from an input point to a hashed point
  val idx = zCurve.index(OrdinalVector(7, 28, 2001, 8))
  println(s"(7, 28, 2001, 8) -> $idx")

  // invert the map back to inputs
  val point = zCurve.inverseIndex(idx)
  println(s"$idx <- $point")
}
