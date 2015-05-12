package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.planners.{SquareQuadTreePlanner, ZCurvePlanner, OffSquareQuadTreePlanner}
import org.eichelberger.sfc.utils.Lexicographics
import Lexicographics.Lexicographic
import org.eichelberger.sfc.SpaceFillingCurve._

import scala.collection.mutable

// algorithm transcribed from pseudo-code contained in
//   "Compact Hilbert Indices for Multi-Dimensional Data"
//   Hamilton, C. H., Rau-Chaplin, A.
//   In Proceedings of the First International Conference on Complex, Intelligent and Software Intensive Systems
//   published by the IEEE.  2007.
//   https://web.cs.dal.ca/~arc/publications/2-43/paper.pdf

// additional (background, inverse index) parts were adapted from
//   C. Hamilton.  Compact Hilbert indices.
//   Technical Report C-2006-07.
//   Dalhousie University, Faculty of Computer Science, July 2006.
//   https://www.cs.dal.ca/sites/default/files/technical_reports/CS-2006-07.pdf

/*
  Working notes...

  n:  the number of dimensions
  m_i:  the precision of dimension i = 2^(m_i) divisions
  h:  the Hilbert index
  p:  the point [p_0, ..., p_(n-1)]
  M:  total number of bits required to represent a point = m_0 + ... + m_(n-1)
  m:  bits in the bounding hypercube = max(m_0, ..., m_(n-1))
  mu:  a mask
  ||mu||:  the number of '1' bits in the binary representation of a non-negative integer (the parity)

  Pro tip:  Each pass through, n bits are stacked on to the index as a kind of
    interval halving.  They have to be popped off the same way.

  Pro tip:  Barrel-shifting works on a fixed window of n bits.
 */

object CompactHilbertCurve {
  def apply(x: OrdinalNumber*): CompactHilbertCurve = new CompactHilbertCurve(OrdinalVector(x: _*))

  case class Mask(value: Long, size: Int) {
    def this(v: Long) = this(v, onBitsIn(v))

    override def toString: String = {
      s"Mask($value=${value.toBinaryString}, $size)"
    }
  }

  case class GrayCodeRankInverse(i: Long, g: Long)
}

abstract class BaseCompactHilbertCurve(val precisions: OrdinalVector) extends SpaceFillingCurve with Lexicographic with LazyLogging {
  import org.eichelberger.sfc.CompactHilbertCurve._

  val name = "H"

  // for any turn through (en|de)coding, no more than n bits can be valid
  val ValidBitsMask: Long = (1L << n) - 1L

  val TotalValidBitsMask: Long = (1L << M) - 1L

  override def toString: String =
    s"CompactHilbert(${precisions.toString})"

  def asBinaryString(x: Long): String = SpaceFillingCurve.asBinaryString(x, n)

  // the mask is named "mu" in the papers
  def extractMask(i: Int, d: Long): Mask = {
    var accMask = Mask(0,0)
    var j = n - 1
    while (j >= 0) {
      if (precisions((j + d) % n) > i)
        accMask = Mask(accMask.value << 1L | 1L, accMask.size + 1)
      else
        accMask = Mask(accMask.value << 1L     , accMask.size)
      j = j - 1
    }
    accMask
  }

  def barrelShiftLeft(x: Long, places: Long): Long = {
    val netPlaces = if (places > n) places % n else places
    ((x << netPlaces) | (x >>> (n - netPlaces))) & ValidBitsMask
  }

  def barrelShiftRight(x: Long, places: Long): Long = {
    val netPlaces = if (places > n) places % n else places
    (x >>> places) | ((x & ((1 << netPlaces) - 1)) << (n - netPlaces))
  }

  def grayCode(w: Long): Long =
    w ^ (w >>> 1L)

  def inverseGrayCode(t: Long): Long = {
    val maxBit: Long = (t >>> (M - 1)) & 1L
    var bitPos = M - 2
    var acc = (maxBit, maxBit)
    while (bitPos >= 0) {
      val newBit = acc._2 ^ bitAt(t, bitPos)
      acc = ((acc._1 << 1L) | newBit, newBit)
      bitPos = bitPos - 1
    }
    acc._1
  }

  def grayCodeRank(mu: Mask, w: Long): Long = {
    var r = 0L
    var j = n - 1
    while (j >= 0) {
      r = bitAt(mu.value, j) match {
        case 0 => r
        case 1 => (r << 1L) | bitAt(w, j)
      }
      
      j = j - 1
    }
    r
  }

  // gci(i) = r
  // g = gc(i)
  def grayCodeRankInverse(mu: Mask, pi: Long, r: Long): GrayCodeRankInverse = {
    var i = 0L
    var g = 0L
    var j = mu.size - 1
    var k = n - 1
    while (k >= 0) {
      if (bitAt(mu.value, k) == 1) {
        i = setBitAt(i, k, bitAt(r, j).toInt)
        g = setBitAt(g, k, ((bitAt(i, k) + bitAt(i, k + 1)) % 2).toInt)
        j = j - 1
      } else {
        g = setBitAt(g, k, bitAt(pi, k).toInt)
        i = setBitAt(i, k, ((bitAt(g, k) + bitAt(i, k + 1)) % 2).toInt)
      }
      k = k - 1
    }
    GrayCodeRankInverse(i, g)
  }

  val notOne = ~1L & TotalValidBitsMask

  // in the paper, simply "e(i)"
  def entry(i: Long): Long =
    if (i == 0) 0
    else grayCode((i - 1) & notOne)

  // g(i) = k, such that gc(i) ^ gc(i+1) = 1<<k, 0 <= i <= 1<<n - 1
  // g(i) = tsb(i)  // "trailing set bits in the binary representation of i"
  // found some "tsb" ideas:
  //   http://en.wikipedia.org/wiki/Bit_Manipulation_Instruction_Sets
  def g(i: Long): Long =
    if (i == 0 || i >= size) 0
    else onBitsIn(i & (~i - 1L) & TotalValidBitsMask)

  // in the paper, simply "d(i)"
  def nextDim(i: Long): Long = i match {
    case 0                 => 0
    case j if (j % 2) == 0 => g(i - 1) % n
    case _                 => g(i) % n
  }

  def T(e: Long, d: Long)(b: Long): Long =
    barrelShiftRight(b ^ e, d)

  // in the papers, written as "T-1(e,d)(b)"
  def invT(e: Long, d: Long)(b: Long): Long =
    T(barrelShiftRight(e, d), n - d)(b)

  /*

  From the CS-2006-07 paper, there is at least one explicit test
  (NB:  This is NOT the exact same algorithm in the follow-on paper):

  n = 2; m = 3; p = [5,6]

  i | l  T(e,d)(l)  w  e(w)  d(w) | e d h
  --+-----------------------------+-------
  - | -      -      -   -     -   | 0 1 0
  2 | 3      3      2   0     1   | 0 1 2
  1 | 2      2      3   3     0   | 3 0 11
  0 | 1      1      1   0     1   | 3 0 45
  *   *      *      *   *     *     * X *

  The "d" values seem wrong.  Suggest:

  i | l  T(e,d)(l)  w  e(w)  d(w) | e d h
  --+-----------------------------+-------
  - | -      -      -   -     -   | 0 0 0   // the given start value for "d" is 0
  2 | 3      3      2   0     1   | 0 0 2   // d <- (d + d(w) + 1) % n = (0 + 1 + 1) % 2 = 2 % 2 = 0
  1 | 2      2      3   3     0   | 3 1 11  // d <- (0 + 0 + 1) % 2 = 1 % 2 = 1
  0 | 1      1      1   0     1   | 3 1 45  // d <- (1 + 1 + 1) %2 = 3 %2 = 1
  *   *      *      *   *     *     * X *

  */

  def index(point: OrdinalVector): OrdinalNumber = {
    var h = 0L
    var e = 0L
    var d = 0L
    var mu: Mask = Mask(0, 0)
    var ell = 0L
    var t = 0L
    var w = 0L
    var r = 0L
    var i = m - 1
    while (i >= 0) {
      mu = extractMask(i, d)
      ell = seq2long(bitsAt(point, i))
      t = barrelShiftRight(ell ^ e, d)
      w = inverseGrayCode(t)
      r = grayCodeRank(mu, w)

      h = (h << mu.size) | r
      e = e ^ barrelShiftLeft(entry(w), d)
      d = (d + nextDim(w) + 1) % n
      
      i = i - 1
    }

    h
  }

  def inverseIndex(h: OrdinalNumber): OrdinalVector = {
    var e = 0L
    var d = 0L
    var k = 0L
    var p = OrdinalVector(Seq.fill(n)(0L):_*)
    var mu = Mask(0, 0)
    var r = 0L
    var w = 0L
    var g = 0L
    var ell = 0L
    var pi = 0L
    var i = m - 1
    var b = 0
    var bitSeq: Seq[Int] = Nil
    var j = 0
    while (i >= 0) {
      mu = extractMask(i, d)
      bitSeq = Nil; b = mu.size; while (b >= 1) {
        bitSeq = bitSeq :+ bitAt(h, (M - k - b).toInt).toInt
        b = b - 1
      }
      r = seq2long(bitSeq)
      k = k + mu.size

      pi = barrelShiftRight(e, d) & ~mu.value & ValidBitsMask

      val wg = grayCodeRankInverse(mu, pi, r)
      w = wg.i
      g = wg.g
      ell = invT(e, d)(g)
      j = 0
      while (j < n) {
        p = p.set(j, setBitAt(p(j), i, bitAt(ell, j).toInt))
        j = j + 1
      }
      e = e ^ barrelShiftLeft(entry(w), d)
      d = (d + nextDim(w) + 1) % n
      
      i = i - 1
    }

    p
  }
}

// sustains old behavior while we experiment with alternate planners
case class CompactHilbertCurve(override val precisions: OrdinalVector) extends BaseCompactHilbertCurve(precisions) with OffSquareQuadTreePlanner with SquareQuadTreePlanner {
  lazy val isSquare = precisions.min == precisions.max

  override def getRangesCoveringQuery(query: Query): Iterator[OrdinalPair] =
    if (isSquare) getRangesCoveringQueryOnSquare(query)
    else getRangesCoveringQueryOffSquare(query)

  private[this] val indexCache = collection.mutable.HashMap[OrdinalVector, OrdinalNumber]()

  def getOrComputeIndex(point: OrdinalVector): OrdinalNumber =
    indexCache.getOrElseUpdate(point, index(point))

  override def getRanges(prefix: OrdinalNumber, precision: Int): Seq[OrdinalPair] = {
    // initialize the full range
    val dimRanges = new collection.mutable.ArrayBuffer[OrdinalPair]()
    cardinalities.toSeq.foreach { c => dimRanges.append(OrdinalPair(0, c - 1L)) }

    // divide the ranges according to the bits already in the prefix
    var numBitsRemaining = precisions
    var bitPos = 0
    var i = 0
    while (i < precision) {
      // adjust this range
      val oldRange = dimRanges(bitPos)
      val bit = (prefix >> (precision - i - 1)) & 1L
      if (bit == 1L) {
        // upper half of the remaining range
        dimRanges(bitPos) = OrdinalPair(
          1L + ((oldRange.max + oldRange.min) >> 1L),
          oldRange.max
        )
      } else {
        // lower half of the remaining range
        dimRanges(bitPos) = OrdinalPair(
          oldRange.min,
          (oldRange.max + oldRange.min) >> 1L
        )
      }

      // decrement this counter
      numBitsRemaining = numBitsRemaining.set(bitPos, numBitsRemaining(bitPos) - 1)

      i = i + 1

      // update bitPos
      bitPos = (bitPos + 1) % n
      while (i < precision && numBitsRemaining(bitPos) < 1) {
        bitPos = (bitPos + 1) % n
      }
    }

    // short-cut for single-point lookup
    if (precision == M) {
      val idx = getOrComputeIndex(OrdinalVector(dimRanges.map(_.min):_*))
      return Seq(OrdinalPair(idx, idx))
    }

    // Hilbert progression is only guaranteed to be contiguous on square sub-blocks

    //val smallestIncrement = 1L << numBitsRemaining.toSeq.filter(_ > 0).min
    val smallestIncrement = 1L << numBitsRemaining.toSeq.min

    // how many steps can you take from the lowest cell in USER space to the highest by this smallest increment?
    val stepsPerDim = dimRanges.map {
      case OrdinalPair(a, b) => (b - a + 1) / smallestIncrement
    }

    // identify the sub-cubes
    val LL = OrdinalVector(dimRanges.map { case OrdinalPair(a, b) => a }:_*)
    val cubeCounts = combinationsIterator(OrdinalVector(stepsPerDim:_*))
    val cubeLLs = cubeCounts.map(steps => {
      OrdinalVector(LL.zipWith(steps).map {
        case (start, numSteps) => start + smallestIncrement * numSteps
      }:_*)
    })

    // short-cut for points
    if (smallestIncrement == 1) {
      val idxSingletons = cubeLLs.map(getOrComputeIndex).map(idx => OrdinalPair(idx, idx))
      idxSingletons.toSeq
    } else {
      // the minimum increment is larger than 1
      val toggles = OrdinalVector(List.fill(n)(2L):_*)
      val cubeRanges = cubeLLs.map(cubeLL => {
        val points = combinationsIterator(toggles).map(toggle => {
          val point = cubeLL.zipWith(toggle).map {
            case (x, factor) => x + (smallestIncrement - 1) * factor
          }
          OrdinalVector(point:_*)
        }).toList
        val idxs = points.map(point => getOrComputeIndex(point))

        OrdinalPair(idxs.min, idxs.max)
      })

      cubeRanges.toSeq
    }
  }
}
