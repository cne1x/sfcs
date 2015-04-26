package org.eichelberger.sfc.study.locality

import org.eichelberger.sfc.examples.composition.contrast.FactoryXYZT
import org.eichelberger.sfc.study.{ColumnSpec, OutputMetadata, MirroredTSV}
import org.eichelberger.sfc.{ComposedCurve, CompactHilbertCurve, ZCurve, RowMajorCurve}
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.utils.LocalityEstimator

object LocalityEstimatorStudy
  extends MirroredTSV(
    "/tmp/locality.tsv",
    OutputMetadata(Seq(
    ColumnSpec("top.curve", isQuoted = true),
    ColumnSpec("curve", isQuoted = true),
    ColumnSpec("dimensions", isQuoted = false),
    ColumnSpec("total.precision", isQuoted = false),
    ColumnSpec("plys", isQuoted = false),
    ColumnSpec("locality", isQuoted = false),
    ColumnSpec("normalized.locality", isQuoted = false),
    ColumnSpec("locality.inv", isQuoted = false),
    ColumnSpec("normalized.locality.inv", isQuoted = false),
    ColumnSpec("sample.size", isQuoted = false),
    ColumnSpec("sample.coverage", isQuoted = false)
    )),
    writeHeader = true
  ) with App {

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
    println(data)
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

  close()
}
