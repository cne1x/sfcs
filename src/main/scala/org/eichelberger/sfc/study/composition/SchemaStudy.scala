package org.eichelberger.sfc.study.composition

import org.eichelberger.sfc.examples.Geohash
import org.eichelberger.sfc.study.composition.CompositionSampleData._
import org.eichelberger.sfc.study.composition.SchemaStudy.TestCase
import org.eichelberger.sfc.utils.Timing
import org.eichelberger.sfc._
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.study._
import org.joda.time.{DateTimeZone, DateTime}

/*

Need the ability to force the planner to bail out early,
because we've declared in advance that we don't want more
than RRR ranges returned.

Need a new metric:
  Something to represent the percentage of (expected) false-positives
  that result from the current range (approximation).

 */

object SchemaStudy extends App {

  val output: OutputDestination = {
    val columns = OutputMetadata(Seq(
      ColumnSpec("when", isQuoted = true),
      ColumnSpec("curve", isQuoted = true),
      ColumnSpec("test.type", isQuoted = true),
      ColumnSpec("label", isQuoted = true),
      ColumnSpec("precision", isQuoted = false),
      ColumnSpec("replications", isQuoted = false),
      ColumnSpec("dimensions", isQuoted = false),
      ColumnSpec("plys", isQuoted = false),
      ColumnSpec("avg.ranges", isQuoted = false),
      ColumnSpec("avg.cells", isQuoted = false),
      ColumnSpec("cells.per.second", isQuoted = false),
      ColumnSpec("cells.per.range", isQuoted = false),
      ColumnSpec("seconds.per.cell", isQuoted = false),
      ColumnSpec("seconds.per.range", isQuoted = false),
      ColumnSpec("score", isQuoted = false),
      ColumnSpec("adj.score", isQuoted = false),
      ColumnSpec("seconds", isQuoted = false)
    ))
    val baseFile = "schema"
    new MultipleOutput(Seq(
      new MirroredTSV(s"/tmp/$baseFile.tsv", columns, writeHeader = true),
      new JSON(s"/tmp/$baseFile.js", columns)
        with FileOutputDestination { def fileName = s"/tmp/$baseFile.js" }
    ))
  }


  import TestLevels._
  val testLevel = Large

  // standard test suite of points and queries
  val n = getN(testLevel)
  val pointQueryPairs = getPointQueryPairs(testLevel)
  val points: Seq[XYZTPoint] = pointQueryPairs.map(_._1)
  val cells: Seq[Cell] = pointQueryPairs.map(_._2)
  val labels: Seq[String] = pointQueryPairs.map(_._3)

  val uniqueLabels = labels.toSet.toSeq

  case class TestCase(curve: ComposedCurve, columnOrder: String)
  val TXY = "T ~ X ~ Y"
  val T0XYT1 = "T0 ~ X ~ Y ~ T1"
  val XYT = "X ~ Y ~ T"

  def testSchema(testCase: TestCase): Unit = {
    val TestCase(curve, columnOrder) = testCase

    println(s"[SCHEMA ${curve.name}]")
    // conduct all queries against this curve
    val results: List[(String, Seq[OrdinalPair], Long)] = pointQueryPairs.map{
      case (point, rawCell, label) =>
        val x = rawCell.dimensions.head
        val y = rawCell.dimensions(1)
        val t = rawCell.dimensions(3)

        val cell = columnOrder match {
          case s if s == TXY =>
            Cell(Seq(t, x, y))
          case s if s == T0XYT1 =>
            Cell(Seq(t, x, y, t))
          case s if s == XYT =>
            Cell(Seq(x, y, t))
          case _ =>
            throw new Exception(s"Unhandled column order:  $columnOrder")
        }

        //@TODO debug!
        //System.out.println(s"TEST:  cell $cell")

        curve.clearCache()

        val (ranges, msElapsed) = Timing.time(() => {
          val itr = curve.getRangesCoveringCell(cell)
          val list = itr.toList
          list
        })

        // compute a net label (only needed for 3D curves)
        val netLabel = label.take(2) + label.takeRight(1)

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
        output.println(data)
    }
  }

  val BasePrecision = 9

  // Geohash dimensions
  val gh = new Geohash(BasePrecision << 1L)
  val subGH0 = new SubDimension[Seq[Any]]("gh0", gh.pointToIndex, gh.M, 0, BasePrecision * 2 / 3)
  val subGH1 = new SubDimension[Seq[Any]]("gh1", gh.pointToIndex, gh.M, BasePrecision * 2 / 3, BasePrecision * 2 / 3)
  val subGH2 = new SubDimension[Seq[Any]]("gh2", gh.pointToIndex, gh.M, BasePrecision * 4 / 3, (BasePrecision << 1L) - BasePrecision * 4 / 3)

  // geographic dimensions
  val dimX = DefaultDimensions.createLongitude(BasePrecision)
  val dimY = DefaultDimensions.createLongitude(BasePrecision)

  // time dimensions
  val dimTime = DefaultDimensions.createNearDateTime(BasePrecision)
  val dimT0 = new SubDimension[DateTime]("t0", dimTime.index, dimTime.precision, 0, dimTime.precision >> 1)
  val dimT1 = new SubDimension[DateTime]("t1", dimTime.index, dimTime.precision, dimTime.precision >> 1, dimTime.precision - (dimTime.precision >> 1))

  def getCurves: Seq[TestCase] = Seq(
    // R(t, gh)
    TestCase(new ComposedCurve(RowMajorCurve(dimTime.precision, gh.M), Seq(dimTime, gh)), TXY),

    // R(gh, t)
    TestCase(new ComposedCurve(RowMajorCurve(gh.M, dimTime.precision), Seq(gh, dimTime)), XYT),

    // R(t0, gh, t1)
    TestCase(new ComposedCurve(RowMajorCurve(dimT0.precision, gh.M, dimT1.precision), Seq(dimT0, gh, dimT1)), T0XYT1),

    // R(t0, Z(x, y, t1))
    TestCase(
      new ComposedCurve(
        RowMajorCurve(dimT0.precision, dimX.precision + dimY.precision + dimT1.precision),
        Seq(dimT0, new ComposedCurve(
          ZCurve(dimX.precision, dimY.precision, dimT1.precision),
          Seq(dimX, dimY, dimT1)))),
        T0XYT1)
  )
  
  getCurves.foreach(testSchema)

  output.close()
}
