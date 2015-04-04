package org.eichelberger.sfc.examples

import org.eichelberger.sfc.utils.Lexicographics
import Lexicographics._
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc._
import org.eichelberger.sfc.Dimensions._


/**
 * Simple example of how to construct a standard Geohash (see geohash.org)
 * from the primitive pieces available in this project.
 */

class Geohash(precisions: Long)
  extends ComposedCurve(
    new ZCurve(OrdinalVector(precisions - (precisions >> 1L), precisions >> 1L)),
    Seq(
      DefaultDimensions.createLongitude(precisions - (precisions >> 1L)),
      DefaultDimensions.createLatitude(precisions >> 1L)
    )
  ) {
}