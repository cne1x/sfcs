package org.eichelberger.sfc.examples

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.GenericTesting
import org.eichelberger.sfc.SpaceFillingCurve._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.specs2.mutable.Specification

@RunWith(classOf[JUnitRunner])
class GeohashTest extends Specification with LazyLogging {
  val bboxCville = (-78.5238, 38.0097, -78.4464, 38.0705)
  val xCville = -78.488407
  val yCville = 38.038668

  "Geohash example" should {
    val geohash = new Geohash(35)

    "encode/decode round-trip for an interior point" >> {
      // encode
      val hash = geohash.encodePointToHash(xCville, yCville)
      hash must equalTo("dqb0muw")

      // decode
      val cell = geohash.decodeHashToPoint(hash)
      println(s"[Geohash example, Charlottesville] POINT($xCville $yCville) -> $hash -> $cell")
      cell._1.contains(xCville) must beTrue
      cell._2.contains(yCville) must beTrue
    }

    "encode/decode properly at the four corners and the center" >> {
      for (x <- Seq(-180.0, 0.0, 180.0); y <- Seq(-90.0, 0.0, 90.0)) {
        // encode
        val hash = geohash.encodePointToHash(x, y)

        // decode
        val cell = geohash.decodeHashToPoint(hash)
        println(s"[Geohash example, extrema] POINT($x $y) -> $hash -> $cell")
        cell._1.contains(x) must beTrue
        cell._2.contains(y) must beTrue
      }

      // degenerate test outcome
      1 must equalTo(1)
    }

    def getCvilleRanges(curve: Geohash): (OrdinalPair, OrdinalPair, Iterator[OrdinalPair]) = {
      val lonIdxRange = OrdinalPair(
        curve.longitude.index(bboxCville._1),
        curve.longitude.index(bboxCville._3)
      )
      val latIdxRange = OrdinalPair(
        curve.latitude.index(bboxCville._2),
        curve.latitude.index(bboxCville._4)
      )
      val query = Query(Seq(OrdinalRanges(lonIdxRange), OrdinalRanges(latIdxRange)))
      (lonIdxRange, latIdxRange, curve.getRangesCoveringQuery(query))
    }
    
    "generate valid selection indexes" >> {
      val (_, _, ranges) = getCvilleRanges(geohash)

      ranges.size must equalTo(90)
    }
    
    "report range efficiency" >> {
      def atPrecision(xBits: OrdinalNumber, yBits: OrdinalNumber): (Long, Long) = {
        val curve = new Geohash(xBits + yBits)
        val (lonRange, latRange, ranges) = getCvilleRanges(curve)
        (lonRange.size * latRange.size, ranges.size.toLong)
      }

      for (dimPrec <- 10 to 25) {
        val ((numCells, numRanges), ms) = GenericTesting.time{ () => atPrecision(dimPrec, dimPrec - 1) }
        println(s"[ranges across scales, Charlottesville] precision (${dimPrec}, ${dimPrec - 1}) -> $numCells / $numRanges = ${numCells / numRanges} in ${ms} milliseconds")
      }

      1 must equalTo(1)
    }
  }

}
