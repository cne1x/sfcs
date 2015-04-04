package org.eichelberger.sfc

import org.eichelberger.sfc.utils.Lexicographics
import Lexicographics.Lexicographic

import scala.collection.immutable.HashMap
import collection.immutable.TreeMap

object SpaceFillingCurve {
  type OrdinalNumber = Long

  case class OrdinalVector(x: OrdinalNumber*) {
    def size = x.size
    def apply(i: Int): OrdinalNumber = x(i)
    def apply(i: OrdinalNumber): OrdinalNumber = x(i.toInt)
    def max: OrdinalNumber = x.max
    def product: OrdinalNumber = x.product
    def numBits: Int = Math.ceil(Math.log(product) / Math.log(2.0)).toInt
    def sum: Long = x.sum
    def zipWith(that: OrdinalVector): Seq[(OrdinalNumber,OrdinalNumber)] = x.zip(that.x)
    def cardinalities = OrdinalVector(x.map(1L << _):_*)
    def toSeq = x.toSeq
    def set(pos: Int, newX: OrdinalNumber): OrdinalVector = {
      val xx = (0 until size).map {
        case i if i == pos => newX
        case i             => x(i)
      }
      OrdinalVector(xx:_*)
    }
    override def toString: String = x.map(_.toString).mkString(", ")
    def reverse: OrdinalVector = OrdinalVector(x.toSeq.reverse:_*)
    def ++(a: OrdinalNumber) = OrdinalVector((x.toSeq ++ Seq(a)):_*)
    def +(v: OrdinalVector) = OrdinalVector((x.toSeq ++ v.toSeq):_*)
  }

  // assume that "min" and "max" will always be inclusive, since they're discrete
  case class OrdinalPair(min: OrdinalNumber, max: OrdinalNumber) {
    def size: OrdinalNumber = max - min + 1L
  }

  // roughly analogous to "query" for the time being
  case class OrdinalRanges(pairs: OrdinalPair*) {
    def size: Int = pairs.size
    def toSeq: Seq[OrdinalPair] = pairs.toSeq
    def iterator: Iterator[OrdinalPair] = pairs.iterator
  }

  case class Query(rangesPerDim: Seq[OrdinalRanges]) {
    def numDimensions = rangesPerDim.size
    def toSeq = rangesPerDim
    def +(that: Query) =
      Query(rangesPerDim ++ that.rangesPerDim)
  }

  def consolidatedRangeIterator(ranges: Iterator[OrdinalPair]) =
    inmemory_consolidatedRangeIterator(ranges)

  def inmemory_consolidatedRangeIterator(ranges: Iterator[OrdinalPair]): Iterator[OrdinalPair] = {
    val emptyStartsMap = TreeMap.empty[OrdinalNumber, OrdinalPair]
    val emptyStopsMap = TreeMap.empty[OrdinalNumber, OrdinalPair]

    val startsMap = ranges.foldLeft((emptyStartsMap, emptyStopsMap))((accs, opair) => accs match {
      case (startsAcc, stopsAcc) =>
        val start = opair.min - 1L
        val stop = opair.min + opair.size
        (startsAcc.contains(stop), stopsAcc.contains(start)) match {
          case (false, false) =>
            (
              startsAcc + (opair.min -> opair),
              stopsAcc + (opair.max -> opair)
            )
          case (false, true)  =>
            val oldStop = stopsAcc(start)
            val oldStart = startsAcc(oldStop.min)
            (
              (startsAcc - oldStart.min) + (oldStart.min -> OrdinalPair(oldStart.min, opair.max)),
              (stopsAcc - oldStop.max) + (opair.max -> OrdinalPair(oldStart.min, opair.max))
            )
          case (true, false)  =>
            val oldStart = startsAcc(stop)
            val oldStop = stopsAcc(oldStart.max)
            (
              (startsAcc - oldStart.min) + (opair.min -> OrdinalPair(opair.min, oldStop.max)),
              (stopsAcc - oldStop.max) + (oldStop.max -> OrdinalPair(opair.min, oldStop.max))
              )
          case (true, true)   =>
            val oldStart = startsAcc(stopsAcc(start).min)
            val oldStop = stopsAcc(startsAcc(stop).max)
            (
              startsAcc - oldStart.min - oldStop.min + (oldStart.min -> OrdinalPair(oldStart.min, oldStop.max)),
              stopsAcc - oldStart.max - oldStop.max + (oldStop.max -> OrdinalPair(oldStart.min, oldStop.max))
            )
        }
    })._1

    startsMap.iterator.map(_._2)
  }

  def instream_consolidatedRangeIterator(ranges: Iterator[OrdinalPair]) = new Iterator[OrdinalPair] {
    var nextSeed: Option[OrdinalPair] = None
    var top = findTop()
    def findTop(): Option[OrdinalPair] = {
      if (ranges.hasNext) {
        var current = nextSeed
        nextSeed = Option(ranges.next())
        if (current.isEmpty) {
          current = nextSeed
          nextSeed = if (ranges.hasNext) Option(ranges.next()) else None
        }
        while (nextSeed.isDefined && (current.get.max + 1L) == nextSeed.get.min) {
          current = Option(OrdinalPair(current.get.min, nextSeed.get.max))
          nextSeed = if (ranges.hasNext) Option(ranges.next()) else None
        }
        current
      } else {
        // there were no more items, so use whatever you pre-fetched
        val result = nextSeed
        nextSeed = None
        result
      }
    }
    def hasNext: Boolean = top.isDefined
    def next(): OrdinalPair = {
      val result = top.getOrElse(throw new Exception("Invalid top"))
      top = findTop()
      result
    }
  }
  
  // returns an iterator over all combinations of ordinal numbers
  def combinationsIterator(counts: OrdinalVector): Iterator[OrdinalVector] = {
    val bounds = counts.toSeq.map(c => OrdinalPair(0, c - 1))
    combinationsIterator(bounds)
  }

  def combinationsIterator(bounds: Seq[OrdinalPair]) = new Iterator[OrdinalVector] {
    val counts = bounds.map(_.size)
    var n = counts.size
    val idx = bounds.map(pair => pair.min).foldLeft(collection.mutable.MutableList[OrdinalNumber]())((acc, b) => {
      acc += b
    })
    var _hasNext = counts.product >= 0L
    def hasNext: Boolean = _hasNext
    def next(): OrdinalVector = {
      val result = (0 until n).map(j => idx(j)).toOrdinalVector
      var i = n - 1
      idx(i) = idx(i) + 1
      while (i >= 0 && idx(i) > bounds(i).max) {
        idx(i) = bounds(i).min
        i = i - 1
        if (i >= 0) idx(i) = idx(i) + 1
      }
      _hasNext = i >= 0
      result
    }
  }

  def rangesCombinationsIterator(rangeSets: Seq[OrdinalRanges]) = new Iterator[OrdinalVector] {
    val counts = rangeSets.map(ranges => ranges.toSeq.map(_.size).sum)
    val n = counts.size
    // initialized to the list of minimum values per dimension
    val extremaPerDim: Seq[OrdinalPair] = rangeSets.map(ranges => OrdinalPair(ranges.toSeq.head.min, ranges.toSeq.head.min))
    val counter = extremaPerDim.map(_.min).foldLeft(collection.mutable.MutableList[OrdinalNumber]())((acc, b) => {
      acc += b
    })
    // initialized to the minimum range index per dimension
    val rangeCounter = rangeSets.map(ranges => 0).foldLeft(collection.mutable.MutableList[OrdinalNumber]())((acc, b) => {
      acc += b
    })
    var _hasNext = counts.product >= 1
    def hasNext: Boolean = _hasNext
    def next(): OrdinalVector = {
      val result = (0 until n).map(j => counter(j)).toOrdinalVector
      var i = n - 1
      counter(i) = counter(i) + 1L
      while (i >= 0 && counter(i) > rangeSets(i).toSeq(rangeCounter(i).toInt).max) {
        rangeCounter(i) = rangeCounter(i) + 1
        if (rangeCounter(i) >= rangeSets(i).size) {
          // this dimension has been exhausted; reset it, and carry one
          rangeCounter(i) = 0
          counter(i) = extremaPerDim(i).min
          i = i - 1
          if (i >= 0) counter(i) = counter(i) + 1
        } else {
          // this dimension is not yet exhausted; pick the minimum of the next range in the dimension
          counter(i) = rangeSets(i).toSeq(rangeCounter(i).toInt).min
        }
      }
      _hasNext = i >= 0
      result
    }
  }

  implicit class long2ordvec(x: OrdinalNumber) {
    def +(v: OrdinalVector) = OrdinalVector((Seq(x) ++ v.x.toSeq):_*)
  }

  implicit class ords2ordvec(xs: Seq[OrdinalNumber]) {
    def toOrdinalVector = OrdinalVector(xs:_*)
  }

  implicit class int2long(x: Int) {
    def toLong = x.toLong
    def toOrdinalNumber = x.toLong
  }

  def bitAt(x: Long, pos: Int): Long = (x >> pos) & 1L

  // return the sequence of bits at the 'pos' position of x
  def bitsAt(xs: OrdinalVector, pos: Int): Seq[Int] =
    (0 until xs.size).map(i => bitAt(xs(i), pos).toInt)

  def setBitAt(x: Long, pos: Int, newBit: Int): Long =
    (x & ~(1 << pos)) | (newBit.toLong << pos)

  def seq2long(bits: Seq[Int]): Long =
    bits.foldRight(0L)((bit, acc) => (acc << 1L) | bit.toLong)

  def onBitsIn(x: Long): Int = (0 to 63).foldLeft((x, 0))((acc, t) => {
    val (accX, accCount) = acc
    if ((accX & 1L) != 0) (accX >> 1, accCount + 1)
    else (accX >> 1, accCount)
  })._2

  def asBinaryString(x: Long, len: Int): String =
    x.toBinaryString.reverse.padTo(len, "0").mkString.reverse

  def lsbsInCommon(a: OrdinalNumber, b: OrdinalNumber):  OrdinalNumber = {
    var i = 0L
    val x = a ^ b
    while (i < 64) {  //@TODO should not be constant here
      if ((x & (1L << i)) == 1) return i
      i = i + 1
    }
    i
  }

  def msbsInCommon(a: OrdinalNumber, b: OrdinalNumber):  OrdinalNumber = {
    var i = 63L  //@TODO should not be constant here
    val x = a ^ b
    while (i > 0) {
    val bit = 1L << i
      if ((x & (1L << i)) == 1) return i
      i = i - 1
    }
    i
  }

  // identifies the full bit-levels (for one dimension) covering
  // the span from the coordinate's minimum to its maximum
  def bitCoverages(coords: OrdinalPair, maxBits: OrdinalNumber): Seq[OrdinalPair] = {
    if (coords.min > coords.max) return Seq[OrdinalPair]()
    if (coords.max == coords.min) return Seq(OrdinalPair(coords.min, 1))
    if ((coords.min & 1L) == 1L) return Seq(OrdinalPair(coords.min, 1)) ++
      bitCoverages(OrdinalPair(coords.min + 1L, coords.max), maxBits)

    val span = coords.max - coords.min
    val deltas = ~coords.min

    // find the largest group
    var i = maxBits
    var b = deltas & ((1L << i) - 1L)
    while (i >= 1 && b > span) {
      i = i - 1
      b = deltas & ((1L << i) - 1L)
    }
    b = b + 1L

    require((b - 1) <= span, s"b - 1(${b - 1}) > span ($span)")

    val lowEnd = onBitsIn(b) match {
      case 0 => throw new Exception(s"Unexpected case for b $b in bitCoverages($coords, $maxBits)")
      case 1 => Seq(OrdinalPair(coords.min, b))  // entire block
      case _ =>
        while (b == (span + 1) && i >= 1) {
          i = i - 1
          b = deltas & ((1L << i) - 1L)
        }
        require((b - 1L) != span, s"Infinite recursion!")
        bitCoverages(OrdinalPair(coords.min, coords.min + b - 1L), maxBits)
    }
    val highEnd =
      if (b > span) Seq[OrdinalPair]()
      else bitCoverages(OrdinalPair(coords.min + b, coords.max), maxBits)

    lowEnd ++ highEnd
  }

  trait Composable {
    def n: Int = 1
    def plys: Int = 1
    def name: String
  }

  case class Cell(dimensions: Seq[Dimension[_]]) {
    def size = dimensions.size
    def contains(values: Seq[_]): Boolean =
      dimensions.zip(values).forall {
        case (dim, value) => dim.containsAny(value)
      }
    def apply(dim: Int) = dimensions(dim)
  }

  trait SpaceFillingCurve extends Composable {
    // the number of bits consumed by each of the constituent dimensions
    def precisions: OrdinalVector

    // the number of dimensions
    override val n = precisions.size

    // the maximum number of bits precision in any single dimension
    val m = precisions.max.toInt

    // the total bits precision summed across all dimensions
    val M = precisions.sum.toInt

    // the total number of indexible cells in the space
    val size = 1L << M.toLong

    // the maximum allowable point-index value in each dimension
    val sizes = precisions.cardinalities

    // how many cells exist in each of the constituent dimensions
    val cardinalities = precisions.cardinalities.toSeq

    def index(point: OrdinalVector): OrdinalNumber

    def inverseIndex(ordinal: OrdinalNumber): OrdinalVector

    def getRangesCoveringQuery(query: Query): Iterator[OrdinalPair]

    def isEverything(query: Query): Boolean = {
      var i = 0
      val qdr = query.rangesPerDim
      while (i < n) {
        val qr = qdr(i)
        if (qr.size != 1) return false
        if (qr.toSeq.head != OrdinalPair(0, sizes(i) - 1L)) return false
        i = i + 1
      }

      // if you get here, you satisfied all dimensions completely
      true
    }
  }

  trait QuadTreeCurve extends SpaceFillingCurve {
    // this method should work for the quad-tree methods
    // (Z-curve and Compact Hilbert), but is too generic to
    // be very efficient, so the specific curves should
    // override this with their own, curve-specific versions
    def getRangesCoveringQuery(query: Query): Iterator[OrdinalPair] = {
      // quick check for "everything"
      if (isEverything(query))
        return Seq(OrdinalPair(0, size - 1L)).iterator

      // for each dimension, partition the ranges into bin pairs
      val covers = (0 until n).par.map(dimension => {
        val dimRanges: OrdinalRanges = query.toSeq(dimension)
        val dimCovers: Seq[OrdinalPair] =
          dimRanges.toSeq.flatMap(range => bitCoverages(range, precisions(dimension)))
        dimCovers
      }).toIndexedSeq

      // cross all of the dimensions, finding the contiguous cubes
      val counts = OrdinalVector(covers.map(_.size.toOrdinalNumber):_*)
      val itr = combinationsIterator(counts)
      val cubes: Iterator[OrdinalRanges] = itr.flatMap(combination => {
        // assemble the list of coverages from the combination
        val coverList = combination.toSeq.zipWithIndex.map {
          case (coord, dimension) => covers(dimension).toSeq(coord.toInt)
        }
        // use the minimum bin-size among these covers
        val incSize = coverList.map(_.max).min
        // cover all of the sub-cubes by this increment
        val lowerLeft = coverList.map(_.min)
        val counts = OrdinalVector(coverList.map(_.max / incSize):_*)
        val cubeCornerItr = combinationsIterator(counts)
        val cubes = cubeCornerItr.map(cubeCorner => {
          OrdinalRanges(lowerLeft.zip(cubeCorner.toSeq).map {
            case (left, factor) =>
              val cubeDimMin = left + incSize * factor
              val cubeDimMax = left + incSize * (factor + 1L) - 1L
              OrdinalPair(cubeDimMin, cubeDimMax)
          }:_*)
        })
        cubes
      })

      def isPoint(cube: OrdinalRanges): Boolean =
        cube.toSeq.head.min == cube.toSeq.head.max

      // for each contiguous cube, find the corners, and return (min, max) as that cube's index range
      val dimFactors: List[OrdinalPair] = List.fill(n)(OrdinalPair(0, 1))
      val ranges: Iterator[OrdinalPair] = cubes.toSeq.par.map(cube => {
        // if this is a point, use it
        if (isPoint(cube)) {
          // this is a single point
          val coord: OrdinalVector = cube.toSeq.map(_.min).toOrdinalVector
          val idx = index(coord)
          OrdinalPair(idx, idx)
        }
        else {
          // this doesn't represent a single point, so consider all corners
          val corners = combinationsIterator(dimFactors).map(vec => {
            vec.toSeq.zipWithIndex.map {
              case (zeroOne, dimension) =>
                val cubeDimRange = cube.toSeq(dimension)
                if (zeroOne == 0L) cubeDimRange.min else cubeDimRange.max
            }.toOrdinalVector
          })
          val indexes = corners.toSeq.par.map(index)
          // pick out the (min, max) among all of these index values
          val extrema = indexes.foldLeft((Long.MaxValue, Long.MinValue))((acc, idx) => acc match {
            case (minSoFar, maxSoFar) =>
              (Math.min(minSoFar, idx), Math.max(maxSoFar, idx))
          })
          OrdinalPair(extrema._1, extrema._2)
        }
      }).toIterator

      consolidatedRangeIterator(ranges)
    }
  }
}

