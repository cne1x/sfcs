package org.eichelberger.sfc.examples.composition.contrast

import java.io.{FileWriter, File, BufferedWriter, PrintWriter}

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.{Dimension, GenericTesting, DefaultDimensions, ComposedCurve}
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

  object TestLevels extends Enumeration {
    type TestLevel = Value
    val Debug, Small, Medium, Large = Value
  }
  import TestLevels._
  val testLevel = Medium

  case class XYZTPoint(x: Double, y: Double, z: Double, t: DateTime)

  case class MinMax[T](min: T, max: T)

  // ensure that all of this discretizing-dimensions share the SAME precision
  val dimLong = DefaultDimensions.createLongitude(10L)
  val dimLat = DefaultDimensions.createLatitude(10L)
  val dimDate = DefaultDimensions.createDateTime(10L)
  val dimAlt = DefaultDimensions.createDimension("z", 0.0, 50000.0, 10L)

  def discretize[T](value: T, dim: Dimension[T]): (T, T) = {
    val idx = dim.index(value)
    val cell = dim.inverseIndex(idx)
    (cell.min, cell.max)
  }

  def getAllMissingVariants(point:  XYZTPoint, cellX: Dimension[Double], cellY: Dimension[Double], cellZ: Dimension[Double], cellT: Dimension[DateTime]): Seq[(XYZTPoint, Cell, String)] = {
    combinationsIterator(OrdinalVector(2, 2, 2, 2)).map(combination => {
      (
        point,
        Cell(Seq(
          if (combination(0) == 0L) dimLong else cellX,
          if (combination(1) == 0L) dimLat  else cellY,
          if (combination(2) == 0L) dimAlt  else cellZ,
          if (combination(3) == 0L) dimDate else cellT
        )),
        (if (combination(0) == 0L) "-" else "X") +
          (if (combination(1) == 0L) "-" else "Y") +
          (if (combination(2) == 0L) "-" else "Z") +
          (if (combination(3) == 0L) "-" else "T")
        )
    }).toSeq
  }

  def getOneMissingVariants(point:  XYZTPoint, cellX: Dimension[Double], cellY: Dimension[Double], cellZ: Dimension[Double], cellT: Dimension[DateTime]): Seq[(XYZTPoint, Cell, String)] = {
    Seq(
      (point, Cell(Seq(cellX, cellY, cellZ, cellT)), "XYZT"),
      (point, Cell(Seq(cellX, cellY, cellZ, dimDate)), "XYZ-"),
      (point, Cell(Seq(cellX, cellY, dimAlt, cellT)), "XY-T"),
      (point, Cell(Seq(cellX, dimLat, cellZ, cellT)), "X-ZT"),
      (point, Cell(Seq(dimLong, cellY, cellZ, cellT)), "-YZT"),
      (point, Cell(Seq(dimLong, dimLat, dimAlt, dimDate)), "----")
    )
  }

  def getAllOrNothingVariants(point:  XYZTPoint, cellX: Dimension[Double], cellY: Dimension[Double], cellZ: Dimension[Double], cellT: Dimension[DateTime]): Seq[(XYZTPoint, Cell, String)] = {
    Seq(
      (point, Cell(Seq(cellX, cellY, cellZ, cellT)), "XYZT"),
      (point, Cell(Seq(dimLong, dimLat, dimAlt, dimDate)), "----")
    )
  }

  // standard test suite of points and queries
  val n = testLevel match {
    case Debug  =>    1
    case Small  =>   10
    case Medium =>  100
    case Large  => 1000
  }
  val pointQueryPairs: Seq[(XYZTPoint, Cell, String)] = {
    val prng = new Random(5771L)
    val MinDate = new DateTime(2010, 1, 1, 0, 0, 0, DateTimeZone.forID("UTC"))
    val MaxDate = new DateTime(2014, 12, 31, 23, 59, 59, DateTimeZone.forID("UTC"))
    (1 to n).flatMap(i => {
      // construct the point
      val x = Math.min(180.0, Math.max(-180.0, -180.0 + 360.0 * prng.nextDouble()))
      val y = Math.min(90.0, Math.max(-90.0, -90.0 + 180.0 * prng.nextDouble()))
      val z = Math.min(50000.0, Math.max(0.0, 50000.0 * prng.nextDouble()))
      val ms = Math.min(MaxDate.getMillis, Math.max(MinDate.getMillis, MinDate.getMillis + (MaxDate.getMillis - MinDate.getMillis) * prng.nextDouble())).toLong
      val t = new DateTime(ms, DateTimeZone.forID("UTC"))
      val point = XYZTPoint(x, y, z, t)
      // construct a query that contains this point
      // (discretized to equivalent precision)
      val (x0: Double, x1: Double) = discretize(x, dimLong)
      val (y0: Double, y1: Double) = discretize(y, dimLat)
      val (z0: Double, z1: Double) = discretize(z, dimAlt)
      val (t0: DateTime, t1: DateTime) = discretize(t, dimDate)
      val cellX = DefaultDimensions.createDimension("x", x0, x1, 0L)
      val cellY = DefaultDimensions.createDimension("y", y0, y1, 0L)
      val cellZ =  DefaultDimensions.createDimension("z", z0, z1, 0L)
      val cellT = DefaultDimensions.createDateTime(t0, t1, 0L)
      testLevel match {
        case Medium | Small =>
          // only report the combinations in which exactly one dimension is missing
          getOneMissingVariants(point, cellX, cellY, cellZ, cellT)
        case Large =>
          // return the combinations with (and without) queries per dimension
          val allEntries = getAllMissingVariants(point, cellX, cellY, cellZ, cellT)
          if (i == 1) allEntries else allEntries.tail
        case _ =>
          // all or nothing
          getAllOrNothingVariants(point, cellX, cellY, cellZ, cellT)
      }
    })
  }
  val points: Seq[XYZTPoint] = pointQueryPairs.map(_._1)
  val cells: Seq[Cell] = pointQueryPairs.map(_._2)
  val labels: Seq[String] = pointQueryPairs.map(_._3)

  val uniqueLabels = labels.toSet.toSeq

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

    true
  }

  def verifyQueryRanges(curve: ComposedCurve, pw: PrintWriter, print: Boolean = false): Boolean = {
    // conduct all queries against this curve
    val results: List[(String, Seq[OrdinalPair], Long)] = pointQueryPairs.map{
      case (point, rawCell, label) =>
        val cell =
          if (curve.numLeafNodes == 4) rawCell
          else Cell(rawCell.dimensions.take(2) ++ rawCell.dimensions.takeRight(1))

        //@TODO debug!
        //println(s"RANGES TEST:  ${curve.name}, $label, ${curve.M}, $cell")

        val (ranges, msElapsed) = time(() => {
          val itr = curve.getRangesCoveringCell(cell)
          val list = itr.toList
          list
        })
        (label, ranges, msElapsed)
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
        pw.println(data.mkString("\t"))
    }

    true
  }

  def writeCharlottesvilleRanges(curve: ComposedCurve, precision: Int): Boolean = {
    val pw = new PrintWriter(new BufferedWriter(new FileWriter(s"/tmp/Charlottesville-${precision.formatted("%02d")}-${curve.name}.tsv")))
    val pw2 = new PrintWriter(new BufferedWriter(new FileWriter(s"/tmp/Charlottesville-${precision.formatted("%02d")}-${curve.name}.txt")))
    val pw3 = new PrintWriter(new BufferedWriter(new FileWriter(s"/tmp/Charlottesville-${precision.formatted("%02d")}-${curve.name}-curve.wkt")))
    pw.println("order\tidx_min\tidx_max\tnum_cells\twkt")

    val queryCell: Cell = Cell(Seq(
      DefaultDimensions.createDimension("x", bboxCville._1, bboxCville._3, 0L),
      DefaultDimensions.createDimension("y", bboxCville._2, bboxCville._4, 0L)
    ))
    val sparse = collection.mutable.HashMap[OrdinalVector, OrdinalNumber]()
    var maxIdx = 0L
    var idxY0 = Long.MaxValue
    var idxY1 = Long.MinValue
    var idxX0 = Long.MaxValue
    var idxX1 = Long.MinValue
    val ranges = curve.getQueryResultRichRanges(queryCell).toList
    ranges.foreach { rrange => {
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

    // dump the curve as WKT segments
    pw3.println(Seq("i", "idxY", "y", "idxX", "x", "idx", "lastx", "lasty", "wkt").mkString("\t"))
    val longitude = DefaultDimensions.createLongitude(curve.precisions(0))
    val latitude = DefaultDimensions.createLatitude(curve.precisions(1))
    def getLatLon(index: OrdinalNumber): (Double, Double) = {
      val OrdinalVector(ix, iy) = curve.inverseIndex(index)
      (
        longitude.inverseIndex(ix).doubleMid,
        latitude.inverseIndex(iy).doubleMid
      )
    }
    var i = 0L
    for (idxY <- idxY0 - 1 to idxY1 + 1) {
      val y = latitude.inverseIndex(idxY).doubleMid
      for (idxX <- idxX0 - 1 to idxX1 + 1) {
        val x = longitude.inverseIndex(idxX).doubleMid
        val idx = curve.index(OrdinalVector(idxX, idxY))
        val (lastx, lasty) = getLatLon(idx - 1L)
        pw3.println(Seq(
          i, idxY, y, idxX, x, idx, lastx, lasty,
          s"LINESTRING($lastx $lasty, $x $y)"
        ).mkString("\t"))
        i = i + 1L
      }
    }

    pw3.close()
    pw2.close()
    pw.close()

    true
  }

  def perCurveTestSuite(curve: ComposedCurve, pw: PrintWriter, print: Boolean = false): Boolean =
    //verifyRoundTrip(curve, pw, print) &&  //@TODO restore!
      verifyQueryRanges(curve, pw, print)

  "the various compositions" should {
    "print scaling results" >> {
      val pw = new PrintWriter(new BufferedWriter(new FileWriter(s"/tmp/composed-curves.tsv")))
      pw.println(Seq(
        "curve",
        "test.type",
        "label",
        "precision",
        "replications",
        "dimensions",
        "plys",
        "avg.ranges",
        "avg.cells",
        "cells.per.second",
        "cells.per.range",
        "seconds.per.cell",
        "seconds.per.range",
        "score",
        "adj.score",
        "seconds"
      ).mkString("\t"))

      val (bitsLow, bitsHigh) = testLevel match {
        case Debug  => (40, 40)
        case Small  => (20, 30)
        case Medium => (25, 35)
        case Large  => (20, 40)
      }

      for (totalPrecision <- bitsLow to bitsHigh) {
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
