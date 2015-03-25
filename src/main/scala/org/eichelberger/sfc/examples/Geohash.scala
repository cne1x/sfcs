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
class Geohash(val precision: Long)
  extends ZCurve(OrdinalVector((precision >> 1L) + precision % 2, precision >> 1L))
  with Lexicographic {

  val bitsLongitude = precisions(0)
  val longitude = DefaultDimensions.createLongitude(bitsLongitude)

  val bitsLatitude = precisions(1)
  val latitude = DefaultDimensions.createLatitude(bitsLatitude)

  def encodePointToHash(lon: Double, lat: Double): String = {
    val lonIdx = longitude.index(lon)
    val latIdx = latitude.index(lat)
    val ordVec = OrdinalVector(lonIdx, latIdx)
    val idx = index(ordVec)
    val lexIdx = lexEncodeIndex(idx)

    lexEncodeIndex(index(OrdinalVector(longitude.index(lon), latitude.index(lat))))
  }

  // remember:  the conversion is lossy; you get a cell back, not a point
  def decodeHashToPoint(hash: String): (Dimension[Double], Dimension[Double]) = {
    val coordinates = inverseIndex(lexDecodeIndex(hash))
    val lon = longitude.inverseIndex(coordinates(0))
    val lat = latitude.inverseIndex(coordinates(1))
    (lon, lat)
  }
}
