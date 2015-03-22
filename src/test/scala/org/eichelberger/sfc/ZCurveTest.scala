package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.SpaceFillingCurve._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.specs2.mutable.Specification

@RunWith(classOf[JUnitRunner])
class ZCurveTest extends Specification with GenericCurveValidation with LazyLogging {
  sequential

  def curveName = "ZCurve"

  def createCurve(precisions: OrdinalNumber*): SpaceFillingCurve =
    new ZCurve(precisions.toOrdinalVector)

  "Z-order space-filling curves" should {
    "satisfy the ordering constraints" >> {
      timeTestOrderings() must beTrue
    }

    "identify ranges from coordinate queries" >> {
      val sfc = ZCurve(OrdinalVector(4, 4))
      val query = Query(Seq(
        OrdinalRanges(OrdinalPair(1, 5)),
        OrdinalRanges(OrdinalPair(6, 12))
      ))
      val ranges = sfc.getRangesCoveringQuery(query).toList
      ranges.zipWithIndex.foreach {
        case (range, i) =>
          println(s"[z-curve query ranges $query] $i:  $range")
      }

      ranges.size must equalTo(11)
      ranges.map(_.size).sum must equalTo(35L)
    }
  }
}
