package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.SpaceFillingCurve._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.specs2.mutable.Specification

@RunWith(classOf[JUnitRunner])
class RectilinearCurveTest extends Specification with GenericCurveValidation with LazyLogging {
  sequential

  def curveName = "RectilinearCurve"

  def createCurve(precisions: OrdinalNumber*): SpaceFillingCurve =
    RectilinearCurve(precisions.toOrdinalVector)

  "rectilinear space-filling curves" should {
    "satisfy the ordering constraints" >> {
      timeTestOrderings() must beTrue
    }

    "identify sub-ranges correctly" >> {
      val sfc = createCurve(3, 3)
      val query = OrdinalRectangle(OrdinalPair(1, 2), OrdinalPair(1, 3))
      val ranges = sfc.getPrefixesCoveringQuery(query).toList

      for (i <- 0 until ranges.size) {
        println(s"[rectilinear ranges:  query $query] range $i = ${ranges(i)}")
      }

      ranges(0) must equalTo(OrdinalPair(9, 11))
      ranges(1) must equalTo(OrdinalPair(17, 19))
    }
  }
}
