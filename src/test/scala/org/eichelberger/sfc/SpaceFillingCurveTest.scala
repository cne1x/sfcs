package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.CompactHilbertCurve.Mask
import org.eichelberger.sfc.SpaceFillingCurve.{OrdinalVector, SpaceFillingCurve, _}
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SpaceFillingCurveTest extends Specification with LazyLogging {
  sequential

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

    "iterate over range-collection combinations correctly" >> {
      val ranges = Seq(
        OrdinalRanges(OrdinalPair(1, 1)),
        OrdinalRanges(OrdinalPair(2, 3)),
        OrdinalRanges(OrdinalPair(4, 5), OrdinalPair(7, 7))
      )
      val itr = rangesCombinationsIterator(ranges)
      var n = 0
      while (itr.hasNext && n < 10) {
        println(s"[range-combinations iterator] n $n:  ${itr.next()}")
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

  "bit coverages" >> {
    def testCoverages(range: OrdinalPair, precision: OrdinalNumber, expected: Seq[OrdinalPair]): Boolean = {
      val result = bitCoverages(range, precision)
      println(s"[bit coverage] range $range, precision $precision...\n${result.mkString("  ","\n  ","")}")
      result must equalTo(expected)
    }

    testCoverages(OrdinalPair(6, 12), 4, Seq(OrdinalPair(6, 2), OrdinalPair(8, 4), OrdinalPair(12, 1))) must beTrue
    testCoverages(OrdinalPair(1, 5), 4, Seq(OrdinalPair(1, 1), OrdinalPair(2, 2), OrdinalPair(4, 2))) must beTrue
    testCoverages(OrdinalPair(0, 65535), 16, Seq(OrdinalPair(0, 65536))) must beTrue
  }

  "cross-curve comparisons" should {
    "be possible" >> {
      val n = 2
      val z = new ZCurve(OrdinalVector(n, n))
      val c = CompactHilbertCurve(OrdinalVector(n, n))
      val r = RowMajorCurve(OrdinalVector(n, n))

      def d0(sfc: SpaceFillingCurve, name: String): Double = {
        val data = for (i <- 0 to sfc.size.toInt - 2) yield {
          val j = i + 1
          val pi = sfc.inverseIndex(i)
          val pix = pi(0)
          val piy = pi(1)
          val pj = sfc.inverseIndex(j)
          val pjx = pj(0)
          val pjy = pj(1)
          val ds = Math.hypot(pix - pjx, piy - pjy)
          val di = Math.abs(i - j)
          ds / di
        }
        val mean = data.sum / data.size.toDouble
        println(s"[mean dSpace/dIndex] $name:  ${mean.formatted("%1.3f")}")
        mean
      }

      val idr = d0(r, "R")
      val idz = d0(z, "Z")
      val idc = d0(c, "C")

      idc must beLessThan(idz)
      idz must beLessThan(idr)
    }
  }
}
