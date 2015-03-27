package org.eichelberger.sfc.utils

import org.eichelberger.sfc.ComposedCurve
import org.eichelberger.sfc.SpaceFillingCurve._

import scala.collection.immutable.HashMap
import scala.collection.mutable

object CompositionWKT {
  case class ContiguousPolygon(curve: ComposedCurve, sparse: HashMap[OrdinalVector,OrdinalNumber]) {
    def isAdjacent(coords: OrdinalVector, idx: OrdinalNumber): Boolean = {
      sparse.contains(OrdinalVector(coords(0) - 1, coords(1))) ||
        sparse.contains(OrdinalVector(coords(0) + 1, coords(1))) ||
        sparse.contains(OrdinalVector(coords(0), coords(1) - 1)) ||
        sparse.contains(OrdinalVector(coords(0), coords(1) + 1))
    }

    def +(item: (OrdinalVector, OrdinalNumber)): ContiguousPolygon =
      ContiguousPolygon(curve, sparse + (item._1 -> item._2))

    def minY = sparse.map { case (k,v) => k(1) }.min
    def maxY = sparse.map { case (k,v) => k(1) }.max
    def minX = sparse.map { case (k,v) => k(0) }.min
    def maxX = sparse.map { case (k,v) => k(0) }.max

    def upperLeft: OrdinalVector = {
      require(sparse.size > 0, "The ContiguousPolygon must contain at least one cell")
      var x = minX
      var y = maxY
      while (!sparse.contains(OrdinalVector(x, y))) {
        x = x + 1
        if (x > maxX) {
          x = minX
          y = y - 1
        }
      }
      OrdinalVector(x, y)
    }

    def wkt: String = {
      // find the left-most cell of the first row with data
      var UL = upperLeft
      var current = OrdinalVector(UL(0), UL(1), 0)
      var seen = mutable.HashSet[OrdinalVector]()
      val sb = new StringBuilder()
      val points = mutable.ArrayBuffer[String]()

      // initialize the starting point
      val cell: Cell = curve.indexToCell(sparse(OrdinalVector(current(0), current(1))))
      points += cell.dimensions(0).min + " " + cell.dimensions(1).max

      // Theseus and the minotaur ensues
      while (!seen.contains(current)) {
        val cell: Cell = curve.indexToCell(sparse(OrdinalVector(current(0), current(1))))
        current(2) match {
          case 0 | 5 =>  // top L->R
            points += cell.dimensions(0).max + " " + cell.dimensions(1).max
          case 1 | 6 =>  // right T->B
            points += cell.dimensions(0).max + " " + cell.dimensions(1).min
          case 2 | 7 =>  // bottom L->R
            points += cell.dimensions(0).min + " " + cell.dimensions(1).min
          case 3 | 4 =>  // left T->B
            points += cell.dimensions(0).min + " " + cell.dimensions(1).max
        }
        seen.add(OrdinalVector(current(0), current(1), current(2)))
        current(2) match {
          case 0 =>  // moving right along the top edge
            if (sparse.contains(OrdinalVector(current(0) + 1, current(1) + 1))) {
              // move up
              current = OrdinalVector(current(0) + 1, current(1) + 1, 3)
            } else if (sparse.contains(OrdinalVector(current(0) + 1, current(1)))) {
              // move right
              current = OrdinalVector(current(0) + 1, current(1), 0)
            } else  {
              // move down
              current = OrdinalVector(current(0), current(1), 1)
            }
          case 1 =>  // moving down the right edge
            if (sparse.contains(OrdinalVector(current(0) + 1, current(1) - 1))) {
              // move right
              current = OrdinalVector(current(0) + 1, current(1) - 1, 0)
            } else if (sparse.contains(OrdinalVector(current(0), current(1) - 1))) {
              // move down
              current = OrdinalVector(current(0), current(1) - 1, 1)
            } else {
              // move left
              current = OrdinalVector(current(0), current(1), 2)
            }
          case 2 =>  // moving left along the bottom edge
            if (sparse.contains(OrdinalVector(current(0) - 1, current(1) - 1))) {
              // move down
              current = OrdinalVector(current(0) - 1, current(1) - 1, 1)
            } else if (sparse.contains(OrdinalVector(current(0) - 1, current(1)))) {
              // move left
              current = OrdinalVector(current(0) - 1, current(1), 2)
            } else {
              // move up
              current = OrdinalVector(current(0), current(1), 3)
            }
          case 3 =>  // moving up the left edge
            if (sparse.contains(OrdinalVector(current(0) - 1, current(1) + 1))) {
              // move left
              current = OrdinalVector(current(0) - 1, current(1) + 1, 2)
            } else if (sparse.contains(OrdinalVector(current(0), current(1) + 1))) {
              // move up
              current = OrdinalVector(current(0), current(1) + 1, 3)
            } else {
              // move left
              current = OrdinalVector(current(0), current(1), 0)
            }
          case 4 =>  // moving left along the top edge
            if (sparse.contains(OrdinalVector(current(0) - 1, current(1) + 1))) {
              // move up
              current = OrdinalVector(current(0) - 1, current(1) + 1, 5)
            } else if (sparse.contains(OrdinalVector(current(0) - 1, current(1)))) {
              // move left
              current = OrdinalVector(current(0) - 1, current(1), 4)
            } else {
              // move up
              current = OrdinalVector(current(0), current(1), 7)
            }
          case 5 =>  // moving up the right edge
            if (sparse.contains(OrdinalVector(current(0) + 1, current(1) + 1))) {
              // move right
              current = OrdinalVector(current(0) + 1, current(1) + 1, 6)
            } else if (sparse.contains(OrdinalVector(current(0), current(1) + 1))) {
              // move up
              current = OrdinalVector(current(0), current(1) + 1, 5)
            } else {
              // move left
              current = OrdinalVector(current(0), current(1), 4)
            }
          case 6 =>  // moving right along the bottom edge
            if (sparse.contains(OrdinalVector(current(0) + 1, current(1) - 1))) {
              // move down
              current = OrdinalVector(current(0) + 1, current(1) - 1, 1)
            } else if (sparse.contains(OrdinalVector(current(0) + 1, current(1)))) {
              // move right
              current = OrdinalVector(current(0) + 1, current(1), 6)
            } else {
              // move up
              current = OrdinalVector(current(0), current(1), 5)
            }
          case 7 =>  // moving down the left edge
            if (sparse.contains(OrdinalVector(current(0) - 1, current(1) - 1))) {
              // move left
              current = OrdinalVector(current(0) - 1, current(1) - 1, 4)
            } else if (sparse.contains(OrdinalVector(current(0), current(1) - 1))) {
              // move down
              current = OrdinalVector(current(0), current(1) - 1, 7)
            } else {
              // move right
              current = OrdinalVector(current(0), current(1), 6)
            }
          case _ => throw new Exception("Fix this; bad direction.")
        }
      }

      points.mkString("((", ", ", "))")
    }
  }

  case class BBox(x0: Double, y0: Double, x1: Double, y1: Double) {
    def expandToInclude(bbox: BBox): BBox = BBox(
      Math.min(x0, bbox.x0),
      Math.min(y0, bbox.y0),
      Math.max(x1, bbox.x1),
      Math.max(y1, bbox.y1)
    )

    def areaDegrees = Math.abs((x1 - x0) * (y1 - y0))

    def wkt: String =
      s"POLYGON(($x0 $y0, $x0 $y1, $x1 $y1, $x1 $y0, $x0 $y0))"
  }

  case class RichRange(order: OrdinalNumber, range: OrdinalPair, wkt: String)

  implicit class CellWKT(cell: Cell) {
    require(cell.size == 2, s"CellWKT is only valid for 2D curves")

    // assumes (X, Y) order
    def bbox: BBox = BBox(
      cell.dimensions(0).doubleMin,
      cell.dimensions(1).doubleMin,
      cell.dimensions(0).doubleMax,
      cell.dimensions(1).doubleMax
    )

    def wkt: String = bbox.wkt
  }

  implicit class ComposedCurveWKT(composedCurve: ComposedCurve) {
    require(composedCurve.n == 2, s"ComposedCurveWKT is only valid for 2D curves")

    def getMultipolygonFromRange(range: OrdinalPair): String = {
      val polys = collection.mutable.ArrayBuffer[ContiguousPolygon]()

      // first point
      val coord: OrdinalVector = composedCurve.inverseIndex(range.min)
      polys += ContiguousPolygon(composedCurve, HashMap[OrdinalVector,OrdinalNumber](coord -> range.min))

      // accumulate subsequent points
      for (idx <- (range.min + 1L) to range.max) {
        val coord = composedCurve.inverseIndex(idx)
        var foundSlot = -1
        for (slot <- 0 until polys.size) {
          if (polys(slot).isAdjacent(coord, idx)) foundSlot = slot
        }
        if (foundSlot > -1) polys(foundSlot) = polys(foundSlot) + (coord, idx)
        else polys += ContiguousPolygon(composedCurve, HashMap[OrdinalVector,OrdinalNumber](coord -> idx))
      }

      polys.size match {
        case 0 => throw new Exception("Must have encountered at least one polygon")
        case 1 => "POLYGON" + polys.head.wkt
        case _ => "MULTIPOLYGON(" + polys.map(_.wkt).mkString(", ") + ")"
      }
    }

    def bboxFromRange(range: OrdinalPair): BBox = {
      // lazy:  iterate over all cells
      var bbox = composedCurve.indexToCell(range.min).bbox
      var idx = range.min + 1L
      while (idx <= range.max) {
        bbox = bbox.expandToInclude(composedCurve.indexToCell(idx).bbox)
        idx = idx + 1L
      }
      bbox
    }

    def getQueryResultRichRanges(cell: Cell): Iterator[RichRange] = {
      val itr = composedCurve.getRangesCoveringCell(cell)
      itr.zipWithIndex.map {
        case (range, i) =>
          RichRange(i, range, getMultipolygonFromRange(range))
      }
    }
  }
}
