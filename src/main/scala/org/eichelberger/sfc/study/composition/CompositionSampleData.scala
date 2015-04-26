package org.eichelberger.sfc.study.composition

import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.{Dimension, DefaultDimensions}
import org.joda.time.{DateTimeZone, DateTime}

import scala.util.Random

case class XYZTPoint(x: Double, y: Double, z: Double, t: DateTime)

case class MinMax[T](min: T, max: T)

object CompositionSampleData {
  object TestLevels extends Enumeration {
    type TestLevel = Value
    val Debug, Small, Medium, Large = Value
  }
  import TestLevels._

  val bboxCville = (-78.5238, 38.0097, -78.4464, 38.0705)

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

  def getN(testLevel: TestLevels.Value) = testLevel match {
    case Debug  =>    1
    case Small  =>   10
    case Medium =>  100
    case Large  => 1000
  }

  def getPointQueryPairs(testLevel: TestLevels.Value): Seq[(XYZTPoint, Cell, String)] = {
    val prng = new Random(5771L)
    val MinDate = new DateTime(2010, 1, 1, 0, 0, 0, DateTimeZone.forID("UTC"))
    val MaxDate = new DateTime(2014, 12, 31, 23, 59, 59, DateTimeZone.forID("UTC"))
    (1 to getN(testLevel)).flatMap(i => {
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

}
