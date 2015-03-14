package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.Lexicographics.Lexicographic
import org.eichelberger.sfc.SpaceFillingCurve._

/**
 * Assumes that the dimensions are listed in order from most
 * significant (first) to least significant (last).
 *
 * If you think about this, it's really just bit-ordering:
 * The most significant bits are first, followed by the less
 * significant bits, and the least significant bits bring up
 * the end.
 */
case class RectilinearCurve(precisions: OrdinalVector) extends SpaceFillingCurve with Lexicographic with LazyLogging {
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
}
