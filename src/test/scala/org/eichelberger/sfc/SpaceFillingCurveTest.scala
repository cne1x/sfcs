package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.SpaceFillingCurve._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.specs2.mutable.Specification

@RunWith(classOf[JUnitRunner])
class SpaceFillingCurveTest extends Specification with LazyLogging {
  "static functions in SFC" should {
    "iterate over counted combinations correctly" >> {
      val bounds = OrdinalVector(1, 2, 3)
      val itr = combinationsIterator(bounds)
      var n = 0
      while (itr.hasNext && n < 10) {
        println(s"[combinations count iterator] n $n:  ${itr.next()}")
        n = n + 1
      }
      n must equalTo(6)
    }

    "iterate over bounded combinations correctly" >> {
      val bounds = Seq(OrdinalPair(0, 0), OrdinalPair(1, 2), OrdinalPair(3, 5))
      val itr = combinationsIterator(bounds)
      var n = 0
      while (itr.hasNext && n < 10) {
        println(s"[combinations bounded iterator] n $n:  ${itr.next()}")
        n = n + 1
      }
      n must equalTo(6)
    }

    "consolidated ranges iteratively" >> {
      val ranges = Seq(
        OrdinalPair(3, 17),
        OrdinalPair(19, 20),
        OrdinalPair(22, 23),
        OrdinalPair(24, 25),
        OrdinalPair(31, 39),
        OrdinalPair(40, 49),
        OrdinalPair(60, 60),
        OrdinalPair(61, 61),
        OrdinalPair(62, 62)
      )

      val consolidated = consolidatedRangeIterator(ranges.iterator).toList
      for (i <- 0 until consolidated.size) {
        println(s"[consolidated range iteration] i $i:  ${consolidated(i)}")
      }
      consolidated.size must equalTo(5)
    }
  }
}
