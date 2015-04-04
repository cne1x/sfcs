package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.utils.Lexicographics
import Lexicographics.Lexicographic
import org.eichelberger.sfc.SpaceFillingCurve._

object RowMajorCurve {
  def apply(x: OrdinalNumber*): RowMajorCurve = new RowMajorCurve(OrdinalVector(x: _*))
}

/**
 * Assumes that the dimensions are listed in order from most
 * significant (first) to least significant (last).
 *
 * If you think about this, it's really just bit-ordering:
 * The most significant bits are first, followed by the less
 * significant bits, and the least significant bits bring up
 * the end.
 */
case class RowMajorCurve(precisions: OrdinalVector) extends SpaceFillingCurve with Lexicographic with LazyLogging {
  import org.eichelberger.sfc.RowMajorCurve._

  val name = "R"

  val bitMasks = precisions.x.map(p => (1L << p) - 1L)

  def index(point: OrdinalVector): OrdinalNumber = {
    var i = 0
    var acc = 0L
    while (i < precisions.size) {
      acc = (acc << precisions(i)) | (point(i) & bitMasks(i))
      i = i + 1
    }
    acc
  }

  def inverseIndex(ordinal: OrdinalNumber): OrdinalVector = {
    var i = precisions.size - 1
    var point = OrdinalVector()
    var ord = ordinal
    while (i >= 0) {
      point = point ++ (ord & bitMasks(i))
      ord = ord >> precisions(i)
      i = i - 1
    }
    point.reverse
  }

  def getRangesCoveringQuery(query: Query): Iterator[OrdinalPair] = {
    // naive:  assume none of the dimensions is full-range
    // (if they are, the range-consolidation should fix it, albeit more slowly
    // than if we handled it up front)

    val allRangeSets: Seq[OrdinalRanges] = query.toSeq
    val lastRangeSet: OrdinalRanges = allRangeSets.last
    val prefinalRangeSets: Seq[OrdinalRanges] =
      if (allRangeSets.size > 1) allRangeSets.dropRight(1)
      else Seq()

    // only consider combinations preceding the last (least significant) dimension
    val itr = rangesCombinationsIterator(prefinalRangeSets)
    val ranges = itr.flatMap(vec => {
      lastRangeSet.toSeq.map(r => {
        val minIdx = index(vec ++ r.min)
        val maxIdx = index(vec ++ r.max)
        OrdinalPair(minIdx, maxIdx)
      })
    })

    // final clean-up
    consolidatedRangeIterator(ranges)
  }
}
