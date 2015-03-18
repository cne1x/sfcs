package org.eichelberger.sfc.utils

import java.io.{PrintStream, PrintWriter}

import org.eichelberger.sfc.SpaceFillingCurve._

trait RenderTarget {
  def beforeRendering(sfc: RenderSource): Unit
  def beforeSlice(sfc: RenderSource, slice: OrdinalVector): Unit
  def beforeRow(sfc: RenderSource, row: OrdinalNumber): Unit
  def beforeCol(sfc: RenderSource, col: OrdinalNumber): Unit
  def renderCell(sfc: RenderSource, index: OrdinalNumber, point: OrdinalVector)
  def afterCol(sfc: RenderSource, col: OrdinalNumber): Unit
  def afterRow(sfc: RenderSource, row: OrdinalNumber): Unit
  def afterSlice(sfc: RenderSource, slice: OrdinalVector): Unit
  def afterRendering(sfc: RenderSource): Unit
}

trait RenderSource {
  this: SpaceFillingCurve =>

  def getCurveName: String

  def indexBounds: Seq[OrdinalPair] =
    precisions.toSeq.map(p => OrdinalPair(0L, (1L << p) - 1L))

  def nonTerminalIndexBounds: Seq[OrdinalPair] =
    if (precisions.size > 2) indexBounds.dropRight(2)
    else Seq(OrdinalPair(0, 0))

  def terminalIndexBounds: Seq[OrdinalPair] =
    indexBounds.takeRight(Math.min(2, precisions.size))

  def render(target: RenderTarget) = {
    target.beforeRendering(this)

    // loop over all dimensions higher than 2
    val slices = combinationsIterator(nonTerminalIndexBounds)
    while (slices.hasNext) {
      // identify the context (combination of dimensions > 2)
      val slice: OrdinalVector = slices.next()
      target.beforeSlice(this, slice)

      // dump this 1- or 2-d slice
      var row = -1L
      val cellItr = combinationsIterator(terminalIndexBounds)
      while (cellItr.hasNext) {
        val cell: OrdinalVector = cellItr.next()
        val fullVec = indexBounds.size match {
          case 1 | 2 => cell
          case _     => slice + cell
        }
        val idx = index(fullVec)

        // switch rows
        if (row != cell.toSeq.head) {
          if (row != -1L) target.afterRow(this, row)
          row = cell.toSeq.head
          target.beforeRow(this, row)
        }

        // print cell
        target.renderCell(this, idx, cell)
      }

      target.afterRow(this, row)

      // finish this slice
      target.afterSlice(this, slice)
    }

    target.afterRendering(this)
  }
}

// dump the layers of the SFC cell-indexes to STDOUT
class ScreenRenderTarget extends RenderTarget {
  val pw: PrintStream = System.out

  var numRows: Long = 0L
  var numCols: Long = 0L

  def beforeRendering(sfc: RenderSource): Unit = {}

  def beforeSlice(sfc: RenderSource, slice: OrdinalVector): Unit = {
    pw.println(s"\n[${sfc.getCurveName}:  SLICE $slice ]")

    val (nr: Long, nc: Long) = sfc.terminalIndexBounds.size match {
      case 0 => throw new Exception("Cannot print an empty SFC")
      case 1 => (1, sfc.terminalIndexBounds(0).max + 1L)
      case 2 => (sfc.terminalIndexBounds(0).max + 1L, sfc.terminalIndexBounds(1).max + 1L)
    }
    numRows = nr; numCols = nc
  }

  def beforeRow(sfc: RenderSource, row: OrdinalNumber): Unit = {
    pw.print(s" ${format(row)} | ")
  }

  def beforeCol(sfc: RenderSource, col: OrdinalNumber): Unit = {}

  def renderCell(sfc: RenderSource, index: OrdinalNumber, point: OrdinalVector): Unit = {
    pw.print(s"${format(index)} | ")
  }

  def afterCol(sfc: RenderSource, col: OrdinalNumber): Unit = {}

  def afterRow(sfc: RenderSource, row: OrdinalNumber): Unit = {
    pw.println()
  }

  def afterSlice(sfc: RenderSource, slice: OrdinalVector): Unit = {
    // print the X axis
    separate(pw, numCols.toInt)
    pw.println()
    pw.print("      | ")
    for (x <- 0 until numCols.toInt) {
      pw.print(s"${format(x)} | ")
    }
    pw.println()
  }

  def afterRendering(sfc: RenderSource): Unit = {}

  def closePrintStream(): Unit = {}

  def format(x: Long): String = x.formatted("%4d")

  def separate(pw: PrintStream, numCols: Int): Unit = {
    val line = "------+"*numCols
    pw.print(s"      +$line")
  }
}

// dump the layers of the SFC cell-indexes to STDOUT suitable for Graphviz rendering
//
// to render correctly:
//   neato -n input.dot -Tpng -o output.png
class GraphvizRenderTarget extends RenderTarget {
  val pw: PrintStream = System.out
  val ptsSpacing = 75

  var numRows: Long = 0L
  var numCols: Long = 0L

  def qw(s: String) = "\"" + s + "\""

  def beforeRendering(sfc: RenderSource): Unit = {
    pw.println("digraph G {")
    pw.println("node [ shape=\"square\" width=\"0.5\" height=\"0.5\" ]")
  }

  def beforeSlice(sfc: RenderSource, slice: OrdinalVector): Unit = {
    pw.println(s"subgraph {")
    pw.println("edge [ constraint=\"false\" ]")
    //pw.println(s"<h1>${sfc.getCurveName}:  SLICE $slice</h1>")

    val (nr: Long, nc: Long) = sfc.terminalIndexBounds.size match {
      case 0 => throw new Exception("Cannot print an empty SFC")
      case 1 => (1, sfc.terminalIndexBounds(0).max + 1L)
      case 2 => (sfc.terminalIndexBounds(0).max + 1L, sfc.terminalIndexBounds(1).max + 1L)
    }
    numRows = nr; numCols = nc
  }

  def beforeRow(sfc: RenderSource, row: OrdinalNumber): Unit = {
  }

  def beforeCol(sfc: RenderSource, col: OrdinalNumber): Unit = {}

  def renderCell(sfc: RenderSource, index: OrdinalNumber, point: OrdinalVector): Unit = {
    if (index >= 1L) pw.println(s"node_${index - 1L} -> node_$index")

    val (x: Long, y: Long) = point.size match {
      case 1 => (point(0) * ptsSpacing, 0L)
      case 2 => (point(1) * ptsSpacing, point(0) * ptsSpacing)
    }

    pw.println(s"node_${index} [ label=${qw(index.toString)} pos=${qw(x.toString + "," + y.toString)} ]")
  }

  def afterCol(sfc: RenderSource, col: OrdinalNumber): Unit = {}

  def afterRow(sfc: RenderSource, row: OrdinalNumber): Unit = {
  }

  def afterSlice(sfc: RenderSource, slice: OrdinalVector): Unit = {
    pw.println(s"}")  // end subgraph
  }

  def afterRendering(sfc: RenderSource): Unit = {
    pw.println("}")  // end graph
  }
}