package org.eichelberger.sfc.study.locality

import org.eichelberger.sfc.examples.composition.contrast.FactoryXYZT
import org.eichelberger.sfc.{ComposedCurve, CompactHilbertCurve, ZCurve, RowMajorCurve}
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.utils.LocalityEstimator

object LocalityStudy extends App {
  val pw = new java.io.PrintWriter(new java.io.BufferedWriter(new java.io.FileWriter("/tmp/locality.tsv")))
  pw.println(Seq(
    "top.curve",
    "curve",
    "dimensions",
    "total.precision",
    "plys",
    "locality",
    "normalized.locality",
    "locality.inv",
    "normalized.locality.inv",
    "sample.size",
    "sample.coverage"
  ).mkString("\t"))

  def test(curve: ComposedCurve): Unit = {
    val loc = LocalityEstimator(curve).locality
    val data = Seq(
      curve.name.take(1),
      curve.name,
      curve.numLeafNodes,
      curve.M,
      curve.plys,
      loc.locality,
      loc.normalizedLocality,
      loc.localityInverse,
      loc.normalizedLocalityInverse,
      loc.sampleSize,
      loc.coverage
    )
    println(data.mkString(", "))
    pw.println(data.mkString("\t"))
  }

  for (totalPrecision <- 4 to 40 by 4) {
    // 4D, horizontal
    FactoryXYZT(totalPrecision, 1).getCurves.foreach(curve => test(curve))

    // 4D, mixed (2, 2)
    FactoryXYZT(totalPrecision, 2).getCurves.foreach(curve => test(curve))

    // 4D, mixed (3, 1)
    FactoryXYZT(totalPrecision, -2).getCurves.foreach(curve => test(curve))

    // 4D, vertical
    FactoryXYZT(totalPrecision, 3).getCurves.foreach(curve => test(curve))

  }

  pw.close()
}
