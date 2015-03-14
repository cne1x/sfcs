package org.eichelberger.sfc.examples

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.specs2.mutable.Specification

@RunWith(classOf[JUnitRunner])
class GeohashTest  extends Specification with LazyLogging {
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
  }

}
