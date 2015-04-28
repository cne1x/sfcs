package org.eichelberger.sfc.examples.composition.contrast

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.ComposedCurve
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.study.composition.CompositionSampleData._
import org.eichelberger.sfc.study.composition.XYZTPoint
import org.eichelberger.sfc.utils.Timing._
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StackingVariantsTest extends Specification with LazyLogging {
  sequential

  import TestLevels._
  val testLevel = Small

  // standard test suite of points and queries
  val n = getN(testLevel)
  val pointQueryPairs = getPointQueryPairs(testLevel)
  val points: Seq[XYZTPoint] = pointQueryPairs.map(_._1)
  val cells: Seq[Cell] = pointQueryPairs.map(_._2)
  val labels: Seq[String] = pointQueryPairs.map(_._3)

  val uniqueLabels = labels.toSet.toSeq

  def verifyRoundTrip(curve: ComposedCurve): Boolean = {
    val (_, msElapsed) = time(() => {
      var i = 0
      while (i < n) {
        val XYZTPoint(x, y, z, t) = points(i)
        val pt =
          if (curve.numLeafNodes == 4) Seq(x, y, z, t)
          else Seq(x, y, t)
        val hash = curve.pointToHash(pt)
        val cell = curve.hashToCell(hash)
        cell.contains(pt) must beTrue
        i = i + 1
      }
    })

    println(s"${curve.name},round-trip,${msElapsed/1000.0},${curve.numLeafNodes}")

    true
  }

  def verifyQueryRanges(curve: ComposedCurve): Boolean = {
    // conduct all queries against this curve
    val results: List[(String, Seq[OrdinalPair], Long)] = pointQueryPairs.map{
      case (point, rawCell, label) =>
        val cell =
          if (curve.numLeafNodes == 4) rawCell
          else Cell(rawCell.dimensions.take(2) ++ rawCell.dimensions.takeRight(1))

        val (ranges, msElapsed) = time(() => {
          val itr = curve.getRangesCoveringCell(cell)
          val list = itr.toList
          list
        })

        // compute a net label (only needed for 3D curves)
        val netLabel = curve.numLeafNodes match {
          case 3 => label.take(2) + label.takeRight(1)
          case 4 => label
          case _ =>
            throw new Exception(s"Something went wrong:  ${curve.numLeafNodes} dimensions found")
        }

        (netLabel, ranges, msElapsed)
    }.toList

    // aggregate by label
    val aggregates = results.groupBy(_._1)
    aggregates.foreach {
      case (aggLabel, group) =>
        var totalCells = 0L
        var totalRanges = 0L
        var totalMs = 0L

        group.foreach {
          case (_, ranges, ms) =>
            totalRanges = totalRanges + ranges.size
            totalCells = totalCells + ranges.map(_.size).sum
            totalMs = totalMs + ms
        }

        val m = group.size.toDouble
        val avgRanges = totalRanges.toDouble / m
        val avgCells = totalCells.toDouble / m
        val seconds = totalMs.toDouble / 1000.0
        val avgCellsPerSecond = totalCells / seconds
        val avgRangesPerSecond = totalRanges / seconds
        val avgCellsPerRange = totalRanges / seconds
        val avgSecondsPerCell = seconds / totalCells
        val avgSecondsPerRange = seconds / totalRanges
        val avgScore = avgCellsPerSecond * avgCellsPerRange
        val avgAdjScore = avgCellsPerSecond * Math.log(1.0 + avgCellsPerRange)

        val data = Seq(
          DateTime.now().toString,
          curve.name,
          "ranges",
          aggLabel,
          curve.M,
          n,
          curve.numLeafNodes,
          curve.plys,
          avgRanges,
          avgCells,
          avgCellsPerSecond,
          avgCellsPerRange,
          avgSecondsPerCell,
          avgSecondsPerRange,
          avgScore,
          avgAdjScore,
          seconds
        )
        println(data.mkString(","))
    }

    true
  }

  def perCurveTestSuite(curve: ComposedCurve): Boolean =
    verifyRoundTrip(curve) && verifyQueryRanges(curve)

  "the various compositions" should {
    "print scaling results" >> {
      val totalPrecision = 24

      // 4D, horizontal
      FactoryXYZT(totalPrecision, 1).getCurves.map(curve => perCurveTestSuite(curve))

      // 4D, mixed (2, 2)
      FactoryXYZT(totalPrecision, 2).getCurves.map(curve => perCurveTestSuite(curve))

      // 4D, mixed (3, 1)
      FactoryXYZT(totalPrecision, -2).getCurves.map(curve => perCurveTestSuite(curve))

      // 4D, vertical
      FactoryXYZT(totalPrecision, 3).getCurves.map(curve => perCurveTestSuite(curve))

      // 3D, horizontal
      FactoryXYT(totalPrecision, 1).getCurves.map(curve => perCurveTestSuite(curve))

      // 3D, mixed
      FactoryXYT(totalPrecision, 2).getCurves.map(curve => perCurveTestSuite(curve))

      1 must equalTo(1)
    }
  }
}
