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
  }
}
