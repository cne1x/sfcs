package org.eichelberger.sfc.planners

import org.eichelberger.sfc.SpaceFillingCurve._


trait RecursiveQuadTreePlanner {
  this: SpaceFillingCurve =>

  def dimExtentLT(a: (Int, OrdinalPair), b: (Int, OrdinalPair)): Boolean =
    a._1 < b._1

  def getRange(prefix: OrdinalNumber, precision: Int): OrdinalPair =
    OrdinalPair(
      prefix << (M - precision),
      ((prefix + 1L) << (M - precision)) - 1L
    )

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

  val dims = (0 until n).toSeq

  def getExtents(prefix: OrdinalNumber, precision: Int): Seq[OrdinalPair] = {
    val remaining = OrdinalVector(getBitsRemainingPerDim(prefix, precision):_*)

    // the prefix, when padded to the full curve precision, represents
    // one corner of the cube

    // the other corners are reachable by adding bits to the prefix

    val toggles = combinationsIterator(OrdinalVector(List.fill(n)(2L):_*)).toList
    val cornerIdxs: Seq[OrdinalNumber] = toggles.map(
      toggleVec => {
        // build up the index value for this corner
        toggleVec.zipWith(remaining).foldLeft(prefix)((idxSoFar, tuple) => tuple match {
          case (toggle, remainder) if toggle > 0 =>
            ((idxSoFar + 1L) << remainder) - 1L
          case (toggle, remainder) =>
            idxSoFar << remainder
        })
      })

    // compute the user-space coordinates for each of the corner index values
    val points = cornerIdxs.map(inverseIndex)

    // extract coordinates per dimension
    val coordsPerDim = points.flatMap(point => point.toSeq.zipWithIndex).groupBy(_._2)

    // extract extrema per dimension, and report them in order
    dims.map(dim => {
      val coords = coordsPerDim(dim).map(_._1)
      OrdinalPair(coords.min, coords.max)
    })
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

    def  recursiveSearch(prefix: OrdinalNumber, precision: Int): Seq[OrdinalPair] = {
      val extents = getExtents(prefix, precision)

      // easy case:  this prefix does not overlap at all with the query
      if (queryIsDisjoint(query, extents))
        return Seq()

      // easy case:  this prefix is entirely contained within the query
      if (queryCovers(query, extents))
        return Seq(getRange(prefix, precision))

      // the prefix does overlap, but only partially, with the query

      // check for precision exhaustion
      if (precision == M)
        return Seq(getRange(prefix, precision))
      require(precision < M, s"Precision overflow:  $precision >= $M")

      // recurse
      val zeroBit = recursiveSearch(prefix << 1L, precision + 1)
      val oneBit = recursiveSearch((prefix << 1L) + 1L, precision + 1)
      Seq(zeroBit, oneBit).flatten
    }

    val ranges = recursiveSearch(0L, 0).iterator

    consolidatedRangeIterator(ranges)
  }
}
