package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.CompactHilbertCurve.Mask
import org.eichelberger.sfc.SpaceFillingCurve.{OrdinalVector, SpaceFillingCurve, _}
import org.eichelberger.sfc.utils.Timing
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ZCurveTest extends Specification with GenericCurveValidation with LazyLogging {
  sequential

  def curveName = "ZCurve"

  def createCurve(precisions: OrdinalNumber*): SpaceFillingCurve =
    new ZCurve(precisions.toOrdinalVector)

  def verifyQuery(curve: ZCurve, query: Query, expected: Seq[OrdinalPair]): Boolean = {
    val ranges = curve.getRangesCoveringQuery(query).toList

    ranges.zipWithIndex.foreach {
      case (range, i) =>
        println(s"[z-curve query ranges $query] $i:  $range -> ${expected(i)}")
        range must equalTo(expected(i))
    }

    ranges.size must equalTo(expected.size)

    true
  }

  "planning methods" should {
    val precisions = OrdinalVector(1, 3, 7)
    val z = new ZCurve(precisions)

    "compute ranges from prefix and precision" >> {
      val range_0_0 = OrdinalPair(0, 2047)
      z.getRange(0, 0) must equalTo(range_0_0)

      val range_3_5 = OrdinalPair(192, 255)
      z.getRange(3, 5) must equalTo(range_3_5)
    }

    "identify supersets" >> {
      val qPair = OrdinalPair(10, 20)
      val iPair_sub = OrdinalPair(10, 15)
      val iPair_ovr_L = OrdinalPair(5, 15)
      val iPair_ovr_R = OrdinalPair(15, 25)
      val iPair_sup = OrdinalPair(5, 25)
      val iPair_disjoint_L = OrdinalPair(0, 5)
      val iPair_disjoint_R = OrdinalPair(25, 30)

      z.isPairSupersetOfPair(qPair, iPair_sub) must beTrue
      z.isPairSupersetOfPair(qPair, iPair_ovr_L) must beFalse
      z.isPairSupersetOfPair(qPair, iPair_ovr_R) must beFalse
      z.isPairSupersetOfPair(qPair, iPair_sup) must beFalse
      z.isPairSupersetOfPair(qPair, iPair_disjoint_L) must beFalse
      z.isPairSupersetOfPair(qPair, iPair_disjoint_R) must beFalse
    }

    "identify disjointedness" >> {
      val qPair = OrdinalPair(10, 20)
      val iPair_sub = OrdinalPair(10, 15)
      val iPair_ovr_L = OrdinalPair(5, 15)
      val iPair_ovr_R = OrdinalPair(15, 25)
      val iPair_sup = OrdinalPair(5, 25)
      val iPair_disjoint_L = OrdinalPair(0, 5)
      val iPair_disjoint_R = OrdinalPair(25, 30)

      z.pairsAreDisjoint(qPair, iPair_sub) must beFalse
      z.pairsAreDisjoint(qPair, iPair_ovr_L) must beFalse
      z.pairsAreDisjoint(qPair, iPair_ovr_R) must beFalse
      z.pairsAreDisjoint(qPair, iPair_sup) must beFalse
      z.pairsAreDisjoint(qPair, iPair_disjoint_L) must beTrue
      z.pairsAreDisjoint(qPair, iPair_disjoint_R) must beTrue
    }

    "identify query coverage and disjointedness" >> {
      val query = Query(Seq(
        OrdinalRanges(OrdinalPair(1, 5)),
        OrdinalRanges(OrdinalPair(6, 12))))

      val xAllLow = OrdinalPair(0, 0)
      val xOverlapLow = OrdinalPair(0, 1)
      val xSpan = OrdinalPair(0, 6)
      val xIdent = OrdinalPair(1, 5)
      val xOverlapHigh = OrdinalPair(5, 10)
      val xAllHigh = OrdinalPair(8, 10)

      val yAllLow = OrdinalPair(0, 0)
      val yOverlapLow = OrdinalPair(0, 6)
      val ySpan = OrdinalPair(5, 13)
      val yIdent = OrdinalPair(6, 12)
      val yOverlapHigh = OrdinalPair(12, 15)
      val yAllHigh = OrdinalPair(13, 15)

      // coverage
      (for (
        xr <- Seq(xAllLow, xOverlapLow, xSpan, xIdent, xOverlapHigh, xAllHigh);
        yr <- Seq(yAllLow, yOverlapLow, ySpan, yIdent, yOverlapHigh, yAllHigh) if xr != xIdent || yr != yIdent;
        test = z.queryCovers(query, Seq(xr, yr))
      ) yield test).forall(!_) must beTrue

      z.queryCovers(query, Seq(xIdent, yIdent)) must beTrue

      // disjointedness
      (for (
        xr <- Seq(xAllLow, xAllHigh);
        yr <- Seq(yAllLow, yOverlapLow, ySpan, yIdent, yOverlapHigh, yAllHigh);
        test = z.queryIsDisjoint(query, Seq(xr, yr))
      ) yield {
          println(s"[Z x TEST DISJOINT] query $query, x-range $xr, y-range $yr, is disjoint? $test")
          test
        }).forall(identity) must beTrue

      (for (
        xr <- Seq(xAllLow, xOverlapLow, xSpan, xIdent, xOverlapHigh, xAllHigh);
        yr <- Seq(yAllLow, yAllHigh);
        test = z.queryIsDisjoint(query, Seq(xr, yr))
      ) yield {
          println(s"[Z y TEST DISJOINT] query $query, x-range $xr, y-range $yr, is disjoint? $test")
          test
        }).forall(identity) must beTrue

      val querySmall = Query(Seq(
        OrdinalRanges(OrdinalPair(1, 3)),
        OrdinalRanges(OrdinalPair(2, 3))))
      z.queryIsDisjoint(query, Seq(OrdinalPair(0, 1), OrdinalPair(0, 1))) must beTrue
    }

    "compute bits remaining" >> {
      // everything
      z.getBitsRemainingPerDim(0, 0) must equalTo(z.precisions.toSeq)
      z.getBitsRemainingPerDim(1, 0) must equalTo(z.precisions.toSeq)

      // knock off two bits
      val remaining_after_2 = Seq[OrdinalNumber](0, 2, 7)
      z.getBitsRemainingPerDim(0, 2) must equalTo(remaining_after_2)
      z.getBitsRemainingPerDim(1, 2) must equalTo(remaining_after_2)

      // knock off five bits
      val remaining_after_5 = Seq[OrdinalNumber](0, 1, 5)
      z.getBitsRemainingPerDim(0, 5) must equalTo(remaining_after_5)
      z.getBitsRemainingPerDim(1, 5) must equalTo(remaining_after_5)
    }

    "compute extents" >> {
      def testExtent(prefix: OrdinalNumber, precision: Int, expected: Seq[OrdinalPair]) = {
        val extent = z.getExtents(prefix, precision)
        println(s"[EXTENT ${z.name}] ($prefix, $precision) -> $extent")
        extent must equalTo(expected)
      }

      testExtent(0, 0, z.cardinalities.toSeq.map(c => OrdinalPair(0, c - 1L)))
      testExtent(3, 2, Seq(OrdinalPair(1, 1), OrdinalPair(4, 7), OrdinalPair(0, 127)))
      testExtent(1023, 9, Seq(OrdinalPair(1, 1), OrdinalPair(7, 7), OrdinalPair(124, 127)))
    }

    "identify ranges from coordinate queries on small, 2D, square-ish spaces" >> {
      verifyQuery(
        ZCurve(OrdinalVector(2, 2)),
        Query(Seq(
          OrdinalRanges(OrdinalPair(2, 3)),
          OrdinalRanges(OrdinalPair(1, 3))
        )),
        Seq(
          OrdinalPair(9, 9),
          OrdinalPair(11, 15)
        )
      ) must beTrue
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

    "simple performance estimate" >> {
      val precisions = OrdinalVector(10, 30)
      val query = Query(Seq(
        OrdinalRanges(OrdinalPair(2, 28), OrdinalPair(101, 159)),
        OrdinalRanges(OrdinalPair(19710507, 20010423))
      ))
      val z = ZCurve(precisions)

      val (_, ms) = Timing.time(() => z.getRangesCoveringQuery(query))
      println(s"[PLANNING PERFORMANCE ESTIMATE] ${ms/1000.0} seconds")

      ms must beGreaterThanOrEqualTo(0L)
    }
  }

  "Z-order space-filling curves" should {
    "satisfy the ordering constraints" >> {
      timeTestOrderings() must beTrue
    }
  }
}
