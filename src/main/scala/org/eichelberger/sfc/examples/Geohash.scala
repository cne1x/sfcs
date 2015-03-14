package org.eichelberger.sfc.examples

import org.eichelberger.sfc.Lexicographics._
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.{Real1DPartitioner, RealDimension, ZCurve}

/**
 * Simple example of how to construct a standard Geohash (see geohash.org)
 * from the primitive pieces available in this project.
 */
case class Geohash(precision: Long)
  extends ZCurve(OrdinalVector((precision >> 1L) + precision % 1, precision >> 1L))
  with Lexicographic {

  val bitsLongitude = precisions(0)
  val dimLongitude = RealDimension(-180.0, isMinIncluded = true, +180.0, isMaxIncluded = true)
  val longitude = Real1DPartitioner(dimLongitude, bitsLongitude)

  val bitsLatitude = precisions(1)
  val dimLatitude = RealDimension(-90.0, isMinIncluded = true, +90.0, isMaxIncluded = true)
  val latitude = Real1DPartitioner(dimLatitude, bitsLatitude)

  def encodePointToHash(lon: Double, lat: Double): String =
    lexEncodeIndex(index(OrdinalVector(longitude.index(lon), latitude.index(lat))))

  // remember:  the conversion is lossy; you get a cell back, not a point
  def decodeHashToPoint(hash: String): (RealDimension, RealDimension) = {
    val coordinates = inverseIndex(lexDecodeIndex(hash))
    val lon = longitude.inverseIndex(coordinates(0))
    val lat = latitude.inverseIndex(coordinates(1))
    (lon, lat)
  }
}
