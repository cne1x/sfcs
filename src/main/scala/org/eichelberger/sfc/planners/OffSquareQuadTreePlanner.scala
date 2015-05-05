package org.eichelberger.sfc.planners

import org.eichelberger.sfc.SpaceFillingCurve._

trait OffSquareQuadTreePlanner {
  this: SpaceFillingCurve =>

  // this method should work for Compact Hilbert, but is too generic to
  // be very efficient

  // this method works well for significantly non-square spaces

  def getRangesCoveringQueryOffSquare(query: Query): Iterator[OrdinalPair] = {
    // quick check for "everything"
    if (isEverything(query))
      return Seq(OrdinalPair(0, size - 1L)).iterator

    // for each dimension, partition the ranges into bin pairs
    val covers = (0 until n).map(dimension => {
      val dimRanges: OrdinalRanges = query.toSeq(dimension)
      val dimCovers: Seq[OrdinalPair] =
        dimRanges.toSeq.flatMap(range => bitCoverages(range, precisions(dimension)))
      dimCovers
    })

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
      val innerCubes = cubeCornerItr.map(cubeCorner => {
        OrdinalRanges(lowerLeft.zip(cubeCorner.toSeq).map {
          case (left, factor) =>
            val cubeDimMin = left + incSize * factor
            val cubeDimMax = left + incSize * (factor + 1L) - 1L
            OrdinalPair(cubeDimMin, cubeDimMax)
        }:_*)
      })
      innerCubes
    })

    def isPoint(cube: OrdinalRanges): Boolean =
      cube.toSeq.head.min == cube.toSeq.head.max

    // for each contiguous cube, find the corners, and return (min, max) as that cube's index range
    val dimFactors: List[OrdinalPair] = List.fill(n)(OrdinalPair(0, 1))
    val ranges: Iterator[OrdinalPair] = cubes.toSeq.map(cube => {
      // if this is a point, use it
      if (isPoint(cube)) {
        // this is a single point
        val coord: OrdinalVector = cube.toSeq.map(_.min).toOrdinalVector
        val idx = index(coord)
        OrdinalPair(idx, idx)
      }
      else {
        // this doesn't represent a single point, but you know that it's
        // square on a power-of-two boundary, so all of the least significant
        // bits are necessarily inside (meaning that you only have to sample
        // one)
        val minSpan = cube.toSeq.map {
          case OrdinalPair(a, b) => b - a + 1
        }.min
        val precisionFree = (Math.round(Math.log(minSpan) / Math.log(2.0)).toInt) << 1
        val point = OrdinalVector(cube.toSeq.map(_.min):_*)
        val idx = index(point)
        val idxBase = idx >>> precisionFree
        OrdinalPair(
          idxBase << precisionFree,
          ((idxBase + 1L) << precisionFree) - 1L
        )
      }
    }).toIterator

    consolidatedRangeIterator(ranges)
  }
}