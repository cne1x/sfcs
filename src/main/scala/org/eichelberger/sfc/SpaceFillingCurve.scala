package org.eichelberger.sfc

import org.eichelberger.sfc.utils.Lexicographics
import Lexicographics.Lexicographic

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
  case class OrdinalPair(min: OrdinalNumber, max: OrdinalNumber)

  // roughly analogous to "query" for the time being
  case class OrdinalRectangle(pairs: OrdinalPair*) {
    def size: Int = pairs.size
    def toSeq: Seq[OrdinalPair] = pairs.toSeq
  }

  def consolidatedRangeIterator(ranges: Iterator[OrdinalPair]) = new Iterator[OrdinalPair] {
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
    val counts = bounds.map(pair => pair.max - pair.min + 1)
    var n = counts.size
    val idx = bounds.map(pair => pair.min).foldLeft(collection.mutable.MutableList[OrdinalNumber]())((acc, b) => {
      acc += b
    })
    var _hasNext = (0 until n).map(i => 0 <= (counts(i) - 1)).find(_ == true).getOrElse(false)
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

  def binDelta(x: OrdinalNumber, maxBits: OrdinalNumber, bits: OrdinalNumber): OrdinalNumber = {
    val mask = (1L << bits) - 1L
    mask - (x & mask)
  }

  def bitCoverages(coords: OrdinalPair, maxBits: OrdinalNumber): Seq[OrdinalPair] = {
    if (coords.min > coords.max) return Seq[OrdinalPair]()
    if (coords.max == coords.min) return Seq(OrdinalPair(coords.min, 1))
    if ((coords.min & 1L) == 1L) return Seq(OrdinalPair(coords.min, 1)) ++
      bitCoverages(OrdinalPair(coords.min + 1L, coords.max), maxBits)

    val span = coords.max - coords.min

    // find the largest group
    var i = maxBits
    var b = binDelta(coords.min, maxBits, i)
    var unfound = b > span
    while (i >= 1 && unfound) {
      i = i - 1
      b = binDelta(coords.min, maxBits, i)
      unfound = b > span
    }

    val lowEnd = onBitsIn(b + 1L) match {
      case 0 => throw new Exception(s"Unexpected case for b $b in bitCoverages($coords, $maxBits)")
      case 1 => Seq(OrdinalPair(coords.min, b + 1L))  // entire block
      case _ => bitCoverages(OrdinalPair(coords.min, coords.min + b), maxBits)
    }
    val highEnd = bitCoverages(OrdinalPair(coords.min + b + 1, coords.max), maxBits)

    lowEnd ++ highEnd
  }

  trait SpaceFillingCurve {
    // the number of bits consumed by each of the constituent dimensions
    def precisions: OrdinalVector

    // the number of dimensions
    val n = precisions.size

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

    def getRangesCoveringQuery(query: OrdinalRectangle): Seq[OrdinalPair]
  }
}

