package org.eichelberger.sfc.study.composition

import java.io.{FileWriter, BufferedWriter, PrintWriter}

import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc._
import org.eichelberger.sfc.study._
import org.eichelberger.sfc.study.composition.CompositionSampleData._
import org.eichelberger.sfc.examples.composition.contrast.{FactoryXY, FactoryXYT, FactoryXYZT}
import org.eichelberger.sfc.utils.Timing
import org.joda.time.{DateTimeZone, DateTime}
import org.eichelberger.sfc.utils.CompositionWKT._

object StackingVariantsStudy extends App {

  import TestLevels._
  val testLevel = Medium

  // standard test suite of points and queries
  val n = getN(testLevel)
  val pointQueryPairs = getPointQueryPairs(testLevel)
  val points: Seq[XYZTPoint] = pointQueryPairs.map(_._1)
  val cells: Seq[Cell] = pointQueryPairs.map(_._2)
  val labels: Seq[String] = pointQueryPairs.map(_._3)

  val uniqueLabels = labels.toSet.toSeq

  def goRoundTrip(curve: ComposedCurve, output: OutputDestination): Boolean = {
    val (_, msElapsed) = Timing.time(() => {
      var i = 0
      while (i < n) {
        val XYZTPoint(x, y, z, t) = points(i)
        val pt =
          if (curve.numLeafNodes == 4) Seq(x, y, z, t)
          else Seq(x, y, t)
        val hash = curve.pointToHash(pt)
        val cell = curve.hashToCell(hash)
        i = i + 1
      }
    })

    output.println(Seq(
      curve.name,
      msElapsed / 1000.0,
      curve.numLeafNodes
    ))

    true
  }

  def computeQueryRanges(curve: ComposedCurve, output: OutputDestination): Boolean = {
    // conduct all queries against this curve
    val results: List[(String, Seq[OrdinalPair], Long)] = pointQueryPairs.map{
      case (point, rawCell, label) =>
        val cell =
          if (curve.numLeafNodes == 4) rawCell
          else Cell(rawCell.dimensions.take(2) ++ rawCell.dimensions.takeRight(1))

        val (ranges, msElapsed) = Timing.time(() => {
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
        output.println(data)
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

  def perCurveTestSuite(curve: ComposedCurve, output_rt: OutputDestination, output_qr: OutputDestination): Boolean =
    goRoundTrip(curve, output_rt) &&
    computeQueryRanges(curve, output_qr)

  def createRoundTripOutput: OutputDestination = {
    val columns = OutputMetadata(Seq(
      ColumnSpec("curve", isQuoted = true),
      ColumnSpec("seconds", isQuoted = false),
      ColumnSpec("dimensions", isQuoted = false)
    ))
    val baseFile = "composed-curves-round-trip"
    new MultipleOutput(Seq(
      new MirroredTSV(s"/tmp/$baseFile.tsv", columns, writeHeader = true),
      new JSON(s"/tmp/$baseFile.js", columns)
        with FileOutputDestination { def fileName = s"/tmp/$baseFile.js" }
    ))
  }

  def createQueryRangesOutput: OutputDestination = {
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
    val baseFile = "composed-curves-query-ranges"
    new MultipleOutput(Seq(
      new MirroredTSV(s"/tmp/$baseFile.tsv", columns, writeHeader = true),
      new JSON(s"/tmp/$baseFile.js", columns)
        with FileOutputDestination { def fileName = s"/tmp/$baseFile.js" }
    ))
  }

  def printScalingResults(): Unit = {
    val (bitsLow, bitsHigh, bitsIncrement) = testLevel match {
      case Debug  => (40, 40, 1)
      case Small  => (20, 30, 10)
      case Medium => (20, 40, 10)
      case Large  => (20, 40, 5)
    }

    val output_rt = createRoundTripOutput
    val output_qr = createQueryRangesOutput

    for (totalPrecision <- bitsLow to bitsHigh by bitsIncrement) {
      // 4D, horizontal
      FactoryXYZT(totalPrecision, 1).getCurves.map(curve => perCurveTestSuite(curve, output_rt, output_qr))

      // 4D, mixed (2, 2)
      FactoryXYZT(totalPrecision, 2).getCurves.map(curve => perCurveTestSuite(curve, output_rt, output_qr))

      // 4D, mixed (3, 1)
      FactoryXYZT(totalPrecision, -2).getCurves.map(curve => perCurveTestSuite(curve, output_rt, output_qr))

      // 4D, vertical
      FactoryXYZT(totalPrecision, 3).getCurves.map(curve => perCurveTestSuite(curve, output_rt, output_qr))

      // 3D, horizontal
      FactoryXYT(totalPrecision, 1).getCurves.map(curve => perCurveTestSuite(curve, output_rt, output_qr))

      // 3D, mixed
      FactoryXYT(totalPrecision, 2).getCurves.map(curve => perCurveTestSuite(curve, output_rt, output_qr))
    }

    output_qr.close()
    output_rt.close()
  }

  def writeCharlottesvilleRanges(): Unit = {
    for (totalPrecision <- 21 to 35 by 2) {
      FactoryXY(totalPrecision).getCurves.map(curve => writeCharlottesvilleRanges(curve, curve.M))
    }
  }

  printScalingResults()
}
