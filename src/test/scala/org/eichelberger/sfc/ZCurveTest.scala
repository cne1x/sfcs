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

  def verifyQuery(curve: ZCurve, query: Query, expected: Seq[OrdinalPair]): Boolean = {
    val ranges = curve.getRangesCoveringQuery(query).toList
    ranges.size must equalTo(expected.size)

    ranges.zipWithIndex.foreach {
      case (range, i) =>
        println(s"[z-curve query ranges $query] $i:  $range -> ${expected(i)}")
        range must equalTo(expected(i))
    }

    true
  }

  "Z-order space-filling curves" should {
    "satisfy the ordering constraints" >> {
      timeTestOrderings() must beTrue
    }

    "identify ranges from coordinate queries on square-ish spaces" >> {
      verifyQuery(
        ZCurve(OrdinalVector(4, 4)),
        Query(Seq(
          OrdinalRanges(OrdinalPair(1, 5)),
          OrdinalRanges(OrdinalPair(6, 12))
        )),
        Seq(
          OrdinalPair(22, 23),
          OrdinalPair(28, 31),
          OrdinalPair(52, 55),
          OrdinalPair(66, 67),
          OrdinalPair(70, 79),
          OrdinalPair(82, 82),
          OrdinalPair(88, 88),
          OrdinalPair(90, 90),
          OrdinalPair(96, 103),
          OrdinalPair(112, 112),
          OrdinalPair(114, 114)
        )
      ) must beTrue
    }

    "identify ranges from coordinate queries on non-square-ish space #1" >> {
      verifyQuery(
        ZCurve(OrdinalVector(2, 4)),
        Query(Seq(
          OrdinalRanges(OrdinalPair(1, 2)),
          OrdinalRanges(OrdinalPair(6, 12))
        )),
        Seq(
          OrdinalPair(14, 15),
          OrdinalPair(24, 28),
          OrdinalPair(38, 39),
          OrdinalPair(48, 52)
        )
      ) must beTrue

      verifyQuery(
        ZCurve(OrdinalVector(2, 4)),
        Query(Seq(
          OrdinalRanges(OrdinalPair(2, 3)),
          OrdinalRanges(OrdinalPair(6, 12))
        )),
        Seq(
          OrdinalPair(38, 39),
          OrdinalPair(46, 52),
          OrdinalPair(56, 60)
        )
      ) must beTrue

      verifyQuery(
        ZCurve(OrdinalVector(2, 4)),
        Query(Seq(
          OrdinalRanges(OrdinalPair(0, 3)),
          OrdinalRanges(OrdinalPair(4, 4))
        )),
        Seq(
          OrdinalPair(4, 4),
          OrdinalPair(12, 12),
          OrdinalPair(36, 36),
          OrdinalPair(44, 44)
        )
      ) must beTrue
    }
  }
}
