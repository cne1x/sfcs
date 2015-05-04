package org.eichelberger.sfc.planners

import org.eichelberger.sfc.SpaceFillingCurve._

import scala.collection.mutable

trait QuadTreePlanner {
  this: SpaceFillingCurve =>

  def getBitsRemainingPerDim(prefix: OrdinalNumber, precision: Int): Seq[OrdinalNumber] = {
    var full = precisions
    var bitPos = 0
    var i = 0
    while (i < precision) {
      // decrement this counter
      full = full.set(bitPos, full(bitPos) - 1)

      i = i + 1

      // update bitPos
      bitPos = (bitPos + 1) % n
      while (i < precision && full(bitPos) < 1) {
        bitPos = (bitPos + 1) % n
      }
    }

    full.toSeq
  }

  def getRange(prefix: OrdinalNumber, precision: Int): OrdinalPair =
    OrdinalPair(
      prefix << (M - precision),
      ((prefix + 1L) << (M - precision)) - 1L
    )

  def getRanges(prefix: OrdinalNumber, precision: Int): Seq[OrdinalPair] =
    Seq(getRange(prefix, precision))

  def getExtents(prefix: OrdinalNumber, precision: Int, cacheOpt: Option[mutable.HashMap[OrdinalNumber, OrdinalVector]] = None): Seq[OrdinalPair] = {
    // quad-trees use interval-halving per bit per dimension

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

    dimRanges
  }

  def isPairSupersetOfPair(qPair: OrdinalPair, iPair: OrdinalPair): Boolean =
    iPair.min >= qPair.min && iPair.max <= qPair.max

  def pairsAreDisjoint(qPair: OrdinalPair, iPair: OrdinalPair): Boolean =
    qPair.max < iPair.min || qPair.min > iPair.max

  def queryCovers(query: Query, extents: Seq[OrdinalPair]): Boolean = {
    query.toSeq.zip(extents).foreach {
      case (dimRanges, ordPair) =>
        if (!dimRanges.toSeq.exists(qPair => isPairSupersetOfPair(qPair, ordPair)))
          return false
    }
    true
  }

  def queryIsDisjoint(query: Query, extents: Seq[OrdinalPair]): Boolean = {
    query.toSeq.zip(extents).foreach {
      case (dimRanges, ordPair) =>
        if (dimRanges.toSeq.forall(qPair => pairsAreDisjoint(qPair, ordPair)))
          return true
    }
    false
  }

  // this method should work for the Z-curve, but is too generic to
  // be very efficient
  override def getRangesCoveringQuery(query: Query): Iterator[OrdinalPair] = {
    // quick check for "everything"
    if (isEverything(query))
      return Seq(OrdinalPair(0, size - 1L)).iterator

    // "prefix" is right-justified

    val cache = collection.mutable.HashMap[OrdinalNumber, OrdinalVector]()

    def  recursiveSearch(prefix: OrdinalNumber, precision: Int): Seq[OrdinalPair] = {
      val extents = getExtents(prefix, precision, Option(cache))

      // easy case:  this prefix does not overlap at all with the query
      if (queryIsDisjoint(query, extents))
        return Seq()

      // easy case:  this prefix is entirely contained within the query
      if (queryCovers(query, extents))
        return getRanges(prefix, precision)

      // the prefix does overlap, but only partially, with the query

      // check for precision exhaustion
      if (precision == M)
        return getRanges(prefix, precision)
      require(precision < M, s"Precision overflow:  $precision >= $M")

      // recurse
      Seq(
        recursiveSearch(prefix << 1L, precision + 1),
        recursiveSearch((prefix << 1L) + 1L, precision + 1)
      ).flatten
    }

    val ranges = recursiveSearch(0L, 0).iterator

    consolidatedRangeIterator(ranges)
  }
}
