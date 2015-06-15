package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.examples.Geohash
import org.joda.time.{DateTimeZone, DateTime}
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DimensionTest extends Specification with LazyLogging {
  "simple dimensions" should {
    "remain consistent round-trip" >> {
      val xDim = DefaultDimensions.createLongitude(18)
      val idx = xDim.index(-78.5238)
      val cell = xDim.inverseIndex(idx)
      cell.contains(-78.5238) must beTrue
    }
  }

  "identity dimensions" should {
    "work at both ends" >> {
      def last(p: OrdinalNumber): OrdinalNumber = (1L << p) - 1L

      val i1 = DefaultDimensions.createIdentityDimension(1)
      i1.index(0) must equalTo(0)
      i1.index(1) must equalTo(1)

      val i10 = DefaultDimensions.createIdentityDimension(10)
      i10.index(0) must equalTo(0)
      i10.index(1023) must equalTo(1023)

      val i20 = DefaultDimensions.createIdentityDimension(20)
      i20.index(0) must equalTo(0)
      i20.index(last(20)) must equalTo(last(20))

      val i60 = DefaultDimensions.createIdentityDimension(60)
      i60.index(0) must equalTo(0)
      i60.index(last(60)) must equalTo(last(60))
    }
  }

  "sub dimensions" should {
    "work for location" >> {
      val gh = new Geohash(35)
      val subDim = new SubDimension[Seq[Any]]("gh(3,2)", gh.pointToIndex, gh.M, 15, 10)

      val point = Seq(-78.5238, 38.0097)

      // full-precision index value
      val parentIdx = gh.pointToIndex(point)

      // sub-precision index portion
      val childIdx = subDim.index(point)

      val expectedChildIdx = (parentIdx >>> 10L) & 31L

      println(s"[SUB-DIM LOCATION] parent $parentIdx, child $childIdx")

      childIdx must equalTo(expectedChildIdx)
    }

    "work for time (above a week)" >> {
      val dim = DefaultDimensions.dimTime

      val d0 = new DateTime(0L, DateTimeZone.forID("UTC"))
      val d1 = d0.plusWeeks(1)
      val deltaIdxWeek = dim.index(d1) - dim.index(d0)
      val bitsInWeek = Math.ceil(Math.log(deltaIdxWeek.toDouble) / Math.log(2.0)).toLong

      val subDim = new SubDimension[DateTime]("YYYYww", dim.index, dim.precision, 0, dim.precision - bitsInWeek)

      val point = new DateTime(2015, 5, 14, 20, 29, 0, DateTimeZone.forID("UTC"))

      // full-precision index value
      val parentIdx = dim.index(point)

      // sub-precision index portion
      val childIdx = subDim.index(point)

      val minDT = dim.inverseIndex(childIdx << bitsInWeek).min
      val maxDT = dim.inverseIndex(((childIdx + 1L) << bitsInWeek) - 1L).max

      println(s"[SUB-DIM ABOVE-WEEK] minDT $minDT, point $point, maxDT $maxDT")

      point.isAfter(minDT) must beTrue
      point.isBefore(maxDT) must beTrue
    }

    "work for time (within a week)" >> {
      val dim = DefaultDimensions.dimTime

      val d0 = new DateTime(0L, DateTimeZone.forID("UTC"))
      val d1 = d0.plusWeeks(1)
      val deltaIdxWeek = dim.index(d1) - dim.index(d0)
      val bitsInWeek = Math.ceil(Math.log(deltaIdxWeek.toDouble) / Math.log(2.0)).toLong

      val subDim = new SubDimension[DateTime]("YYYYww", dim.index, dim.precision, dim.precision - bitsInWeek, bitsInWeek)

      val point = new DateTime(2015, 5, 14, 20, 29, 0, DateTimeZone.forID("UTC"))

      // full-precision index value
      val parentIdx = dim.index(point)

      // sub-precision index portion
      val childIdx = subDim.index(point)

      val returnIdx = ((parentIdx >>> bitsInWeek) << bitsInWeek) | childIdx
      returnIdx must equalTo(parentIdx)
    }
  }
}
