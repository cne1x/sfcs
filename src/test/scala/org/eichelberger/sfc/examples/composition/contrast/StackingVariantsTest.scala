package org.eichelberger.sfc.examples.composition.contrast

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.{GenericTesting, DefaultDimensions, ComposedCurve}
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.examples.composition.contrast._
import org.joda.time.{Months, DateTime, DateTimeZone}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.specs2.mutable.Specification

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class StackingVariantsTest extends Specification with LazyLogging {
  sequential

  import GenericTesting._

//  val xPrecision = 18
//  val yPrecision = 17
//  val zPrecision = 10
//  val tPrecision = 15
  val xPrecision = 10
  val yPrecision = 10
  val zPrecision = 10
  val tPrecision = 10

  val xyPrecision = xPrecision + yPrecision

  case class XYZTPoint(x: Double, y: Double, z: Double, t: DateTime)

  case class MinMax[T](min: T, max: T)

  // standard test suite of points and queries
  val n = 100  //@TODO increase this again!
  val pointQueryPairs: Seq[(XYZTPoint, Cell)] = {
    val prng = new Random(5771L)
    val MinDate = new DateTime(2010, 1, 1, 0, 0, 0, DateTimeZone.forID("UTC"))
    val MaxDate = new DateTime(2014, 12, 31, 23, 59, 59, DateTimeZone.forID("UTC"))
    (1 to n).map(i => {
      // construct the point
      val x = Math.min(180.0, Math.max(-180.0, -180.0 + 360.0 * prng.nextDouble()))
      val y = Math.min(90.0, Math.max(-90.0, -90.0 + 180.0 * prng.nextDouble()))
      val z = Math.min(50000.0, Math.max(0.0, 50000.0 * prng.nextDouble()))
      val ms = Math.min(MaxDate.getMillis, Math.max(MinDate.getMillis, MinDate.getMillis + (MaxDate.getMillis - MinDate.getMillis) * prng.nextDouble())).toLong
      val t = new DateTime(ms, DateTimeZone.forID("UTC"))
      val point = XYZTPoint(x, y, z, t)
      // construct a query that contains this point
      val x0 = Math.min(179.0, Math.floor(x))
      val y0 = Math.min(89.0, Math.floor(y))
      val z0 = Math.min(49000.0, 1000.0 * Math.floor(z / 1000.0))
      val t0 = new DateTime(t.getYear, t.getMonthOfYear, 1, 0, 0, 0, DateTimeZone.forID("UTC"))
      val cell = Cell(Seq(
        DefaultDimensions.createDimension(x0, x0 + 1.0, 1L),
        DefaultDimensions.createDimension(y0, y0 + 1.0, 1L),
        DefaultDimensions.createDimension(z0, z0 + 1000.0, 1L),
        DefaultDimensions.createDateTime(t0, t0.plus(Months.months(1)), 1L)
      ))
      // return this pair
      (point, cell)
    })
  }
  val points: Seq[XYZTPoint] = pointQueryPairs.map(_._1)
  val cells: Seq[Cell] = pointQueryPairs.map(_._2)

  def verifyRoundTrip(xyzt: ComposedCurve, print: Boolean = false): Boolean = {
    val (_, msElapsed) = time(() => {
      var i = 0
      while (i < n) {
        val XYZTPoint(x, y, z, t) = points(i)
        val hash = xyzt.pointToHash(Seq(x, y, z, t))
        val cell = xyzt.hashToCell(hash)
        if (print) println(s"[${xyzt.getClass.getSimpleName} verify round trip] ${points(i)} -> $hash -> $cell")
        cell.contains(Seq(x, y, z, t)) must beTrue
        i = i + 1
      }
    })

    //println(s"[${xyzt.getClass.getSimpleName} verify round trip] total time:  ${msElapsed/1000.0} seconds")
    println(s"${xyzt.getClass.getSimpleName},round-trip,${msElapsed/1000.0}")

    true
  }

  def verifyQueryRanges(xyzt: ComposedCurve, print: Boolean = false): Boolean = {
    var totalCells = 0L
    var totalRanges = 0L

    val (_, msElapsed) = time(() => {
      var i = 0
      while (i < n) {
        val cell = cells(i)
        val ranges = xyzt.getRangesCoveringCell(cell).toList
        totalRanges = totalRanges + ranges.size
        totalCells = totalCells + ranges.map(_.size).sum
        if (print) println(s"[${xyzt.getClass.getSimpleName} verify query ranges] ${points(i)} -> $cell -> ${ranges.size} ranges")
        i = i + 1
      }
    })

    val avgRanges = (totalRanges.toDouble / n.toDouble).formatted("%1.2f")
    val avgCells = (totalCells.toDouble / n.toDouble).formatted("%1.2f")

    //println(s"[${xyzt.getClass.getSimpleName} verify query ranges] ${xyzt.M} bits, $n points, $avgRanges avg ranges, $avgCells avg cells, total time:  ${msElapsed/1000.0} seconds")
    println(s"${xyzt.getClass.getSimpleName},queries,${xyzt.M},$n,$avgRanges,$avgCells,${msElapsed/1000.0}")

    true
  }

  def perCurveTestSuite(curve: ComposedCurve, print: Boolean = false): Boolean = {
    //verifyRoundTrip(curve, print) &&
      verifyQueryRanges(curve, print)
  }

  "the various compositions" should {
    //val xyzt = new XY_C__Z_R__T_R(xPrecision, yPrecision, zPrecision, tPrecision)
    //val xyzt = new XYZT_C(xPrecision, yPrecision, zPrecision, tPrecision)

    "print scaling results" >> {
      //for (basePrecision <- 5 to 11) {
      for (basePrecision <- 8 to 11) {
        // purely horizontal
        perCurveTestSuite(new XYZT_C(basePrecision, basePrecision, basePrecision, basePrecision))
        perCurveTestSuite(new XYZT_Z(basePrecision, basePrecision, basePrecision, basePrecision))
        perCurveTestSuite(new XYZT_R(basePrecision, basePrecision, basePrecision, basePrecision))

        // two Hilbert curves joined by a single curve
        perCurveTestSuite(new XY_C__ZT_C__C(basePrecision, basePrecision, basePrecision, basePrecision))
        perCurveTestSuite(new XY_C__ZT_C__Z(basePrecision, basePrecision, basePrecision, basePrecision))
        perCurveTestSuite(new XY_C__ZT_C__R(basePrecision, basePrecision, basePrecision, basePrecision))

        // purely vertical
        perCurveTestSuite(new XY_C__Z_R__T_R(basePrecision, basePrecision, basePrecision, basePrecision))
      }

      1 must equalTo(1)
    }
  }
}
