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
    "encode/decode round-trip for known locations" >> {
      
    }
  }

}
