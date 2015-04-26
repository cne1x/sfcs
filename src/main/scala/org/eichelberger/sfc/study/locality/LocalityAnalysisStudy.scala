package org.eichelberger.sfc.study.locality

import org.eichelberger.sfc.SpaceFillingCurve.{OrdinalVector, SpaceFillingCurve}
import org.eichelberger.sfc.utils.Timing
import org.eichelberger.sfc.{CompactHilbertCurve, ZCurve}

import scala.collection.immutable.HashMap
import scala.collection.mutable

object LocalityAnalysisStudy extends App {
  val padding = 10

  trait InverseLocalityPredictor {
    this: SpaceFillingCurve =>
    def predictInverseLocalitySquareMap: Map[Long, Long]
    def predictInverseLocality: Double
  }

  trait ZInverseLocalityPredictor extends InverseLocalityPredictor {
    this: ZCurve =>

    def predictInverseLocalitySquareMap: Map[Long, Long] = {
      val p = mutable.HashMap[Long, Long]()

      for (bitpos <- M - 1 to 0 by -1) {
        val count = 1L << (M - bitpos - 1)

        val idx: Long = 1L << bitpos
        val prevIdx: Long = idx - 1L

        val coords = inverseIndex(idx)
        val prevCoords = inverseIndex(prevIdx)

        val dist = coords.zipWith(prevCoords).map {
          case (coord, prevCoord) => (coord - prevCoord) * (coord - prevCoord)
        }.sum

        p.put(dist, p.getOrElse(dist, 0L) + count)
      }

      p.toMap
    }

    def predictInverseLocality: Double = {
      val squaredMap = predictInverseLocalitySquareMap

      val (totalDist, totalCount) = squaredMap.foldLeft((0.0, 0.0))((acc, entry) => entry match {
        case (distSquared, count) =>
          (acc._1 + Math.sqrt(distSquared) * count, acc._2 + count)
      })

      totalDist / totalCount
    }


  }

  def exhaustCounts(curve: SpaceFillingCurve): Map[Long, Long] = {
    val counts = new mutable.HashMap[Long, Long]()

    for (idx <- 0 until curve.size.toInt - 1) {
      val a = curve.inverseIndex(idx)
      val b = curve.inverseIndex(idx + 1L)

      val dSquared: Long = a.zipWith(b).map {
        case (coordA, coordB) => (coordA - coordB) * (coordA - coordB)
      }.sum

      counts.put(dSquared, 1L + counts.getOrElse(dSquared, 0L))
    }

    counts.toMap
  }

  def oneCurve(curve: SpaceFillingCurve with InverseLocalityPredictor): Unit = {

    val (counts, msCount) = Timing.time { () => exhaustCounts(curve) }

    val (prediction, msPrediction) = Timing.time { () => curve.predictInverseLocalitySquareMap }

    println(s"\n\n==========[ $curve ]==========\n")
    println(
      "D^2".formatted(s"%${padding}s") + "  " +
        "COUNT".formatted(s"%${padding}s") + "  " +
        "PRED".formatted(s"%${padding}s") + "  " +
        "ERR".formatted(s"%${padding}s"))
    println(
      "-"*padding + "  " +
        "-"*padding + "  " +
        "-"*padding + "  " +
        "-"*padding + "  ")

    val keys = (counts.keySet ++ prediction.keySet).toList.sorted
    for (key <- keys) {
      val c = counts.getOrElse(key, 0L)
      val p = prediction.getOrElse(key, 0L)
      val e = Math.abs(c - p)
      val cs = if (c == 0) "-" else c.toString
      val ps = if (p == 0) "-" else p.toString
      val es = if (e == 0) "-" else e.toString

      println(
        key.formatted(s"%${padding}d") + "  " +
        cs.formatted(s"%${padding}s") + "  " +
        ps.formatted(s"%${padding}s") + "  " +
        es.formatted(s"%${padding}s")
      )

      require(e == 0, "Non-zero error for prediction on $curve")
    }

    println(s"\nInverse locality:  ${curve.predictInverseLocality.formatted("%1.4f")}")

    println(s"\nTimes:\n  count:       ${msCount.formatted("%6d")} ms\n  prediction:  ${msPrediction.formatted("%6d")} ms")
  }

  // compare strategies for various curves (for correctness)
  for (i <- 1 to 6) {
    oneCurve(new ZCurve(OrdinalVector(i, i, i)) with ZInverseLocalityPredictor)  //@TODO broken
    oneCurve(new ZCurve(OrdinalVector(i, i)) with ZInverseLocalityPredictor)
    oneCurve(new ZCurve(OrdinalVector(i, 1)) with ZInverseLocalityPredictor)
    oneCurve(new ZCurve(OrdinalVector(1, i)) with ZInverseLocalityPredictor)
    oneCurve(new ZCurve(OrdinalVector(i, i, 1)) with ZInverseLocalityPredictor)  //@TODO broken
    oneCurve(new ZCurve(OrdinalVector(i, 1, i)) with ZInverseLocalityPredictor)  //@TODO broken
    oneCurve(new ZCurve(OrdinalVector(1, i, i)) with ZInverseLocalityPredictor)  //@TODO broken
  }

  // compare strategies for various curves (for timing)
  println()
  for (i <- 1 to 10) {
    val curve = new ZCurve(OrdinalVector(i, i)) with ZInverseLocalityPredictor
    val (counts, msCount) = Timing.time { () => exhaustCounts(curve) }
    val (prediction, msPrediction) = Timing.time { () => curve.predictInverseLocality }
    println(s"SMALL-SCALE COMPARATIVE TIMING, $curve, $msCount, $msPrediction, ${prediction.formatted("%1.4f")}")
  }

  // drive prediction harder, and see how long it takes
  println()
  for (i <- 13 to 15; j <- 13 to 15; k <- 13 to 15; m <- 13 to 15) {
    val curve = new ZCurve(OrdinalVector(i, j, k, m)) with ZInverseLocalityPredictor
    val (prediction, msPrediction) = Timing.time { () => curve.predictInverseLocality }
    println(s"AT-SCALE PREDICTION TIMING, $curve, $msPrediction, ${prediction.formatted("%1.4f")}")
  }
}
