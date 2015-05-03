package org.eichelberger.sfc.planners

import org.eichelberger.sfc.SpaceFillingCurve._

trait SquareQuadTreePlanner {
  this: SpaceFillingCurve =>

  // this method should work for Compact Hilbert, but is too generic to
  // be very efficient

  // imagine you asked yourself, "What is one of the slowest ways
  // to identify query ranges that does not involve exhaustively
  // iterating over all possible coordinate intersections?", and
  // then wrote this code in answer

  override def getRangesCoveringQuery(query: Query): Iterator[OrdinalPair] = {
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