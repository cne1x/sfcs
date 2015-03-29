package org.eichelberger.sfc.examples.composition.contrast

import java.io.{FileWriter, File, BufferedWriter, PrintWriter}

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.{GenericTesting, DefaultDimensions, ComposedCurve}
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.examples.composition.contrast._
import org.joda.time.{Months, DateTime, DateTimeZone}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.specs2.mutable.Specification
import org.eichelberger.sfc.utils.CompositionWKT._

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class StackingVariantsTest extends Specification with LazyLogging {
  sequential

  import GenericTesting._

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
        DefaultDimensions.createDimension("x", x0, x0 + 1.0, 1L),
        DefaultDimensions.createDimension("y", y0, y0 + 1.0, 1L),
        DefaultDimensions.createDimension("z", z0, z0 + 1000.0, 1L),
        DefaultDimensions.createDateTime(t0, t0.plus(Months.months(1)), 1L)
      ))
      // return this pair
      (point, cell)
    })
  }
  val points: Seq[XYZTPoint] = pointQueryPairs.map(_._1)
  val cells: Seq[Cell] = pointQueryPairs.map(_._2)

  def verifyRoundTrip(curve: ComposedCurve, pw: PrintWriter, print: Boolean = false): Boolean = {
    val (_, msElapsed) = time(() => {
      var i = 0
      while (i < n) {
        val XYZTPoint(x, y, z, t) = points(i)
        val pt =
          if (curve.numLeafNodes == 4) Seq(x, y, z, t)
          else Seq(x, y, t)
        val hash = curve.pointToHash(pt)
        val cell = curve.hashToCell(hash)
        if (print) println(s"[${curve.getClass.getSimpleName} verify round trip] ${points(i)} -> $hash -> $cell")
        cell.contains(pt) must beTrue
        i = i + 1
      }
    })

    println(s"${curve.name},round-trip,${msElapsed/1000.0},${curve.numLeafNodes}")
    pw.println(s"${curve.name}\tround-trip\t${msElapsed/1000.0}\t${curve.numLeafNodes}")

    true
  }

  def verifyQueryRanges(curve: ComposedCurve, pw: PrintWriter, print: Boolean = false): Boolean = {
    var totalCells = 0L
    var totalRanges = 0L

    val (_, msElapsed) = time(() => {
      var i = 0
      while (i < n) {
        val cell =
          if (curve.numLeafNodes == 4) cells(i)
          else Cell(cells(i).dimensions.take(2) ++ cells(i).dimensions.takeRight(1))
        val ranges = curve.getRangesCoveringCell(cell).toList
        totalRanges = totalRanges + ranges.size
        totalCells = totalCells + ranges.map(_.size).sum
        if (print) println(s"[${curve.name} verify query ranges] ${points(i)} -> $cell -> ${ranges.size} ranges")
        i = i + 1
      }
    })

    val avgRanges = (totalRanges.toDouble / n.toDouble).formatted("%1.2f")
    val avgCells = (totalCells.toDouble / n.toDouble).formatted("%1.2f")

    println(s"${curve.name},queries,${curve.M},$n,$avgRanges,$avgCells,${msElapsed/1000.0},${curve.numLeafNodes}")
    pw.println(s"${curve.name}\tqueries\t${curve.M}\t$n\t$avgRanges\t$avgCells\t${msElapsed/1000.0}\t${curve.numLeafNodes}")

    true
  }

  def writeCharlottesvilleRanges(curve: ComposedCurve, precision: Int): Boolean = {
    val pw = new PrintWriter(new BufferedWriter(new FileWriter(s"/tmp/Charlottesville-${precision.formatted("%02d")}-${curve.name}.tsv")))
    val pw2 = new PrintWriter(new BufferedWriter(new FileWriter(s"/tmp/Charlottesville-${precision.formatted("%02d")}-${curve.name}.txt")))
    pw.println("order\tidx_min\tidx_max\tnum_cells\twkt")

    val queryCell: Cell = Cell(Seq(
      DefaultDimensions.createDimension("x", bboxCville._1, bboxCville._3, 1L),
      DefaultDimensions.createDimension("y", bboxCville._2, bboxCville._4, 1L)
    ))
    val sparse = collection.mutable.HashMap[OrdinalVector, OrdinalNumber]()
    var maxIdx = 0L
    var idxY0 = Long.MaxValue
    var idxY1 = Long.MinValue
    var idxX0 = Long.MaxValue
    var idxX1 = Long.MinValue
    curve.getQueryResultRichRanges(queryCell).foreach { rrange => {
      pw.println(s"${rrange.order}\t${rrange.range.min}\t${rrange.range.max}\t${rrange.range.size}\t${rrange.wkt}")
      for (idx <- rrange.range.min to rrange.range.max) {
        val coords: OrdinalVector = curve.inverseIndex(idx)
        sparse.put(coords, idx)
        idxY0 = Math.min(idxY0, coords(1))
        idxX0 = Math.min(idxX0, coords(0))
        idxY1 = Math.max(idxY1, coords(1))
        idxX1 = Math.max(idxX1, coords(0))
      }
    }}
    maxIdx = Math.max(idxX1, idxY1)

    // dump the grid of indexes
    val len = maxIdx.toString.length
    def fmt(ord: OrdinalNumber): String = ord.formatted("%" + len + "d")
    for (y <- idxY1 to idxY0 by -1) {
      for (x <- idxX0 to idxX1) {
        val coords = OrdinalVector(x, y)
        sparse.get(coords).foreach(idx => {
          if (x > idxX0) pw2.print("\t")
          pw2.print(fmt(idx))
        })
      }
      pw2.println()
    }

    pw2.close()
    pw.close()

    true
  }

  def perCurveTestSuite(curve: ComposedCurve, pw: PrintWriter, print: Boolean = false): Boolean = {
    verifyRoundTrip(curve, pw, print) &&
      verifyQueryRanges(curve, pw, print)
  }

  "the various compositions" should {
    "print scaling results" >> {
      val pw = new PrintWriter(new BufferedWriter(new FileWriter(s"/tmp/composed-curves.tsv")))

      for (totalPrecision <- 20 to 30) {
        // 4D, horizontal
        FactoryXYZT(totalPrecision, 1).getCurves.map(curve => perCurveTestSuite(curve, pw))

        // 4D, mixed
        FactoryXYZT(totalPrecision, 2).getCurves.map(curve => perCurveTestSuite(curve, pw))

        // 4D, vertical
        FactoryXYZT(totalPrecision, 3).getCurves.map(curve => perCurveTestSuite(curve, pw))

        // 3D, horizontal
        FactoryXYT(totalPrecision, 1).getCurves.map(curve => perCurveTestSuite(curve, pw))

        // 3D, mixed
        FactoryXYT(totalPrecision, 2).getCurves.map(curve => perCurveTestSuite(curve, pw))
      }

      pw.close()

      1 must equalTo(1)
    }

    "dump query ranges to CSV" >> {
      for (totalPrecision <- 21 to 35 by 2) {
        FactoryXY(totalPrecision).getCurves.map(curve => writeCharlottesvilleRanges(curve, curve.M))
      }

      1 must equalTo(1)
    }
  }
}
