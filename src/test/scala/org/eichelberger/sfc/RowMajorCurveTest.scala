package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.CompactHilbertCurve.Mask
import org.eichelberger.sfc.SpaceFillingCurve.{OrdinalVector, SpaceFillingCurve, _}
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RowMajorCurveTest extends Specification with GenericCurveValidation with LazyLogging {
  sequential

  def curveName = "RowmajorCurve"

  def createCurve(precisions: OrdinalNumber*): SpaceFillingCurve =
    RowMajorCurve(precisions.toOrdinalVector)

  "rowmajor space-filling curves" should {
    "satisfy the ordering constraints" >> {
      timeTestOrderings() must beTrue
    }

    "identify sub-ranges correctly" >> {
      val sfc = createCurve(3, 3)
      val query = Query(Seq(OrdinalRanges(OrdinalPair(1, 2)), OrdinalRanges(OrdinalPair(1, 3))))
      val ranges = sfc.getRangesCoveringQuery(query).toList

      for (i <- 0 until ranges.size) {
        println(s"[rowmajor ranges:  query $query] range $i = ${ranges(i)}")
      }

      ranges(0) must equalTo(OrdinalPair(9, 11))
      ranges(1) must equalTo(OrdinalPair(17, 19))
    }
  }
}
