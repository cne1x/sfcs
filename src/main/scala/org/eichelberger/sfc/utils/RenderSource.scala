package org.eichelberger.sfc.utils

import java.awt.Color
import java.io.{PrintStream, PrintWriter}

import org.eichelberger.sfc.SpaceFillingCurve._

trait RenderTarget {
  def beforeRendering(sfc: RenderSource): Unit = {}
  def beforeSlice(sfc: RenderSource, slice: OrdinalVector): Unit = {}
  def beforeRow(sfc: RenderSource, row: OrdinalNumber): Unit = {}
  def beforeCol(sfc: RenderSource, col: OrdinalNumber): Unit = {}
  def renderCell(sfc: RenderSource, index: OrdinalNumber, point: OrdinalVector): Unit = {}
  def afterCol(sfc: RenderSource, col: OrdinalNumber): Unit = {}
  def afterRow(sfc: RenderSource, row: OrdinalNumber): Unit = {}
  def afterSlice(sfc: RenderSource, slice: OrdinalVector): Unit = {}
  def afterRendering(sfc: RenderSource): Unit = {}
}

trait RenderSource {
  this: SpaceFillingCurve =>

  def getCurveName: String

  def numCells: Long = size

  def useSlices: Boolean = true

  def indexBounds: Seq[OrdinalPair] =
    precisions.toSeq.map(p => OrdinalPair(0L, (1L << p) - 1L))

  def nonTerminalIndexBounds: Seq[OrdinalPair] =
    if (precisions.size > 2) indexBounds.dropRight(2)
    else Seq(OrdinalPair(0, 0))

  def terminalIndexBounds: Seq[OrdinalPair] =
    indexBounds.takeRight(Math.min(2, precisions.size))

  def renderSlices(target: RenderTarget) = {
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
  }

  def renderWhole(target: RenderTarget) = {
    // dump this 1- or 2-d slice
    var row = -1L
    val cellItr = combinationsIterator(indexBounds)
    while (cellItr.hasNext) {
      val cell: OrdinalVector = cellItr.next()
      val idx = index(cell)

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
  }

  def render(target: RenderTarget) = {
    target.beforeRendering(this)

    if (useSlices) renderSlices(target)
    else renderWhole(target)

    target.afterRendering(this)
  }
}

// dump the layers of the SFC cell-indexes to STDOUT
class ScreenRenderTarget extends RenderTarget {
  val pw: PrintStream = System.out

  var numRows: Long = 0L
  var numCols: Long = 0L

  override def beforeRendering(sfc: RenderSource): Unit = {}

  override def beforeSlice(sfc: RenderSource, slice: OrdinalVector): Unit = {
    pw.println(s"\n[${sfc.getCurveName}:  SLICE $slice ]")

    val (nr: Long, nc: Long) = sfc.terminalIndexBounds.size match {
      case 0 => throw new Exception("Cannot print an empty SFC")
      case 1 => (1, sfc.terminalIndexBounds(0).max + 1L)
      case 2 => (sfc.terminalIndexBounds(0).max + 1L, sfc.terminalIndexBounds(1).max + 1L)
    }
    numRows = nr; numCols = nc
  }

  override def beforeRow(sfc: RenderSource, row: OrdinalNumber): Unit = {
    pw.print(s" ${format(row)} | ")
  }

  override def renderCell(sfc: RenderSource, index: OrdinalNumber, point: OrdinalVector): Unit = {
    pw.print(s"${format(index)} | ")
  }

  override def afterRow(sfc: RenderSource, row: OrdinalNumber): Unit = {
    pw.println()
  }

  override def afterSlice(sfc: RenderSource, slice: OrdinalVector): Unit = {
    // print the X axis
    separate(pw, numCols.toInt)
    pw.println()
    pw.print("      | ")
    for (x <- 0 until numCols.toInt) {
      pw.print(s"${format(x)} | ")
    }
    pw.println()
  }

  def format(x: Long): String = x.formatted("%4d")

  def separate(pw: PrintStream, numCols: Int): Unit = {
    val line = "------+"*numCols
    pw.print(s"      +$line")
  }
}

object ShadeRampEndpoint {
  def apply(index: OrdinalNumber, color: Color): ShadeRampEndpoint = {
    val (hue, saturation, brightness) = {
      var arr: Array[Float] = null
      Color.RGBtoHSB(color.getRed, color.getGreen, color.getBlue, arr)
      (arr(0), arr(1), arr(2))
    }

    new ShadeRampEndpoint(index, hue, saturation, brightness)
  }
}
import ShadeRampEndpoint._

case class ShadeRampEndpoint(index: OrdinalNumber, hue: Float, saturation: Float, brightness: Float)

case class ShadeRamp(endLow: ShadeRampEndpoint, endHigh: ShadeRampEndpoint) {
  val indexSpan = endHigh.index - endLow.index
  val slopeHue = (endHigh.hue - endLow.hue) / indexSpan.toDouble
  val slopeSaturation = (endHigh.saturation - endLow.saturation) / indexSpan.toDouble
  val slopeBrightness = (endHigh.brightness - endLow.brightness) / indexSpan.toDouble

  val InvisibleColor = new Color(0, 0, 0, 0)

  def getColor(index: OrdinalNumber): Color = {
    if (index < endLow.index) return InvisibleColor
    if (index > endHigh.index) return InvisibleColor

    val dist = index - endLow.index
    val h = endLow.hue + dist * slopeHue
    val s = endLow.saturation + dist * slopeSaturation
    val b = endLow.brightness + dist * slopeBrightness
    Color.getHSBColor(h.toFloat, s.toFloat, b.toFloat)
  }

  def toHexByte(i: Int): String =
    (if (i < 16) "0" else "") + java.lang.Integer.toHexString(i)

  def getColorHex(index: OrdinalNumber): String = {
    val color = getColor(index)
    toHexByte(color.getRed) + toHexByte(color.getGreen) + toHexByte(color.getBlue) + toHexByte(color.getAlpha)
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

  val drawNumbers = true
  val drawArrows = true
  val cellShadingRamp: Option[ShadeRamp] = None

  def qw(s: String) = "\"" + s + "\""

  override def beforeRendering(sfc: RenderSource): Unit = {
    pw.println("// to render correctly:\n//  neato -n input.dot -Tpng -o output.png")
    pw.println("digraph G {")
    pw.println("\toutputorder=\"nodesfirst\"")
    pw.println("\tnode [ shape=\"square\" width=\"1.058\" labelloc=\"c\" fontsize=\"30\" color=\"#000000\"]")
  }

  override def beforeSlice(sfc: RenderSource, slice: OrdinalVector): Unit = {
    pw.println(s"\n\tsubgraph {")
    pw.println("\tedge [ constraint=\"false\" tailclip=\"false\" headclip=\"false\" color=\"#000000FF\" ]")
    //pw.println(s"<h1>${sfc.getCurveName}:  SLICE $slice</h1>")  //@TODO resolve how to print slice titles

    val (nr: Long, nc: Long) = sfc.terminalIndexBounds.size match {
      case 0 => throw new Exception("Cannot print an empty SFC")
      case 1 => (1, sfc.terminalIndexBounds(0).max + 1L)
      case 2 => (sfc.terminalIndexBounds(0).max + 1L, sfc.terminalIndexBounds(1).max + 1L)
    }
    numRows = nr; numCols = nc
  }

  override def renderCell(sfc: RenderSource, index: OrdinalNumber, point: OrdinalVector): Unit = {
    if (drawArrows)
      if (index >= 1L) pw.println(s"\t\tnode_${index - 1L} -> node_$index")

    val (x: Long, y: Long) = point.size match {
      case 1 => (point(0) * ptsSpacing, 0L)
      case 2 => (point(1) * ptsSpacing, point(0) * ptsSpacing)
    }

    val shading = cellShadingRamp.map(ramp =>
      "style=\"filled\" fillcolor=\"#" + ramp.getColorHex(index) + "\"").getOrElse("")
    val label = "label=\"" + (if (drawNumbers) index.toString else "") + "\""
    pw.println(s"\t\tnode_$index [ $label pos=${qw(x.toString + "," + y.toString)} $shading ]")
  }

  override def afterSlice(sfc: RenderSource, slice: OrdinalVector): Unit = {
    pw.println(s"\t}")  // end subgraph
  }

  override def afterRendering(sfc: RenderSource): Unit = {
    pw.println("}")  // end graph
  }
}


// write an include file suitable for use in a larger
// Persistence of Vision Raytracer scene
//
// only position information is written; no styling
class PovrayRenderTarget extends RenderTarget {
  val pw: PrintStream = System.out

  var povCurveName: String = "UNKNOWN"

  def qw(s: String) = "\"" + s + "\""

  override def beforeRendering(sfc: RenderSource): Unit = {
    povCurveName = sfc.getCurveName.replaceAll("[^a-zA-Z0-9]", "_").toUpperCase

    pw.println("// include file for POV-Ray")
    pw.println(s"// curve name:  ${sfc.getCurveName}")

    pw.println("\n// curve_cells[CellNo][0]:  X coord")
    pw.println("// curve_cells[CellNo][1]:  Y coord")
    pw.println("// curve_cells[CellNo][2]:  Z coord")

    pw.println(s"\n#declare num_curve_cells = ${sfc.numCells};")

    pw.println(s"\n#declare curve_cells = array[${sfc.numCells}][3];")
  }

  override def renderCell(sfc: RenderSource, index: OrdinalNumber, point: OrdinalVector): Unit = {
    val items = (0 to 2).map { i => s"#declare curve_cells[$index][$i] = ${point(i)};" }
    pw.println(items.mkString("  "))
  }
}