package org.eichelberger.sfc.utils

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import BitManipulations._

@RunWith(classOf[JUnitRunner])
class BitManipulationsTest extends Specification with LazyLogging {
  "static methods" should {
    "usedMask" >> {
      // single bits
      for (pos <- 0 to 62) {
        val v = 1L << pos.toLong
        val actual = usedMask(v)
        val expected = (1L << (pos + 1L)) - 1L
        println(s"[usedMask single bit]  pos $pos, value $v, actual $actual, expected $expected")
        actual must equalTo(expected)
      }

      // full bit masks
      for (pos <- 0 to 62) {
        val expected = (1L << (pos.toLong + 1L)) - 1L
        val actual = usedMask(expected)
        println(s"[usedMask full bit masks]  pos $pos, value $expected, actual $actual, expected $expected")
        actual must equalTo(expected)
      }

      usedMask(0) must equalTo(0)
    }

    "sharedBitPrefix" >> {
      sharedBitPrefix(2, 3) must equalTo(2)
      sharedBitPrefix(178, 161) must equalTo(160)
    }

    "common block extrema" >> {
      commonBlockMin(178, 161) must equalTo(160)
      commonBlockMax(178, 161) must equalTo(191)
    }
  }
}
