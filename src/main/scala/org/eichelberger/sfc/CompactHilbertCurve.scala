package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.Lexicographics.Lexicographic
import org.eichelberger.sfc.SpaceFillingCurve._

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

case class CompactHilbertCurve(precisions: OrdinalVector) extends SpaceFillingCurve with Lexicographic with LazyLogging {
  import org.eichelberger.sfc.CompactHilbertCurve._

  // for any turn through (en|de)coding, no more than n bits can be valid
  val ValidBitsMask: Long = (1L << n) - 1L

  override def toString: String =
    s"CompactHilbert(${precisions.toString})"

  def asBinaryString(x: Long): String = SpaceFillingCurve.asBinaryString(x, n)

  // the mask is named "mu" in the papers
  def extractMask(i: Int, d: Long): Mask = {
    var j = n - 1
    var accMask = Mask(0,0)
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
    var j = n - 1
    var r = 0L
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

  // in the paper, simply "e(i)"
  def entry(i: Long): Long = {
    if (i == 0) 0
    else {
      val j = ((i - 1) >>> 1) << 1
      grayCode(j)
    }
  }

  // in the paper, simply "d(i)"
  def nextDim(i: Long): Long = i match {
    case 0                 => 0
    case j if (j % 2) == 0 => inverseGrayCode(i - 1) % n
    case _                 => inverseGrayCode(i) % n
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
    // sanity check
    require(point.size == n, s"You cannot index a point whose rank (${point.size}) is not equal to the space's rank ($n).")
    var ii = 0; while (ii < n) {
      val xi = point(ii)
      val maxSize = sizes(ii)
      require(xi < maxSize, s"You cannot index a dimension whose $ii-th dimension value ($xi) exceeds the cardinality for that dimension ($maxSize).")
      ii = ii + 1
    }

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
    while (i >= 0) {
      mu = extractMask(i, d)
      r = seq2long((mu.size to 1 by -1).map(b => bitAt(h, (M - k - b).toInt).toInt))
      k = k + mu.size

      pi = barrelShiftRight(e, d) & ~mu.value & ValidBitsMask

      val wg = grayCodeRankInverse(mu, pi, r)
      w = wg.i
      g = wg.g
      ell = invT(e, d)(g)
      for (j <- 0 until n) {
        p = p.set(j, setBitAt(p(j), i, bitAt(ell, j).toInt).toInt)
      }
      e = e ^ barrelShiftLeft(entry(w), d)
      d = (d + nextDim(w) + 1) % n
      
      i = i - 1
    }

    p
  }
}

