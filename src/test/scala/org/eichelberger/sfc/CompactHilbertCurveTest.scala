package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.CompactHilbertCurve.Mask
import org.eichelberger.sfc.SpaceFillingCurve.{OrdinalVector, SpaceFillingCurve, _}
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CompactHilbertCurveTest extends Specification with GenericCurveValidation with LazyLogging {
  sequential

  "static functions" should {
    "extract bits" >> {
      val bits = Seq(1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1)
      val bitString = bits.reverse.map(_.toString).mkString("")
      val num = java.lang.Integer.parseInt(bitString, 2)
      for (bit <- 0 until bits.size) {
        println(s"[extract bits] num $num bit $bit known ${bits(bit)} computed ${bitAt(num, bit)}")
        bitAt(num, bit) must equalTo(bits(bit))
      }

      // degenerate test
      1 must equalTo(1)
    }

    "set individual bits" >> {
      // 000...00011010
      (0 to 29).foreach {
        case 1 =>
          setBitAt(26L, 1, 1) must equalTo(26L)
          setBitAt(26L, 1, 0) must equalTo(24L)
        case 3 =>
          setBitAt(26L, 3, 1) must equalTo(26L)
          setBitAt(26L, 3, 0) must equalTo(18L)
        case 4 =>
          setBitAt(26L, 4, 1) must equalTo(26L)
          setBitAt(26L, 4, 0) must equalTo(10L)
        case i =>
          (26L ^ setBitAt(26L, i, 1)) must equalTo(1L << i)
          setBitAt(26L, i, 0) must equalTo(26L)
      }

      // degenerate test
      1 must equalTo(1)
    }

    "onBitsIn must work" >> {
      (1 to 63).foreach { i => {
        onBitsIn((1L << i) - 1L) must equalTo(i)
      }}

      onBitsIn(0L) must equalTo(0)
      onBitsIn(1L) must equalTo(1)
      onBitsIn(256L) must equalTo(1)
      onBitsIn(65536L) must equalTo(1)
      onBitsIn(3L) must equalTo(2)
      onBitsIn(5L) must equalTo(2)
      onBitsIn(65537L) must equalTo(2)
    }

    "instantiate from variable-length lists" >> {
      CompactHilbertCurve(2) must not beNull;
      CompactHilbertCurve(2, 3) must not beNull;
      CompactHilbertCurve(2, 3, 4) must not beNull
    }

    "round-trip Gray (en|de)code" >> {
      val ch = CompactHilbertCurve(3, 2)

      for(w <- 31 to 0 by -1) {
        val t = ch.grayCode(w)
        val w2 = ch.inverseGrayCode(t)
        println(s"[Gray (en|de)coding] w $w t $t w2 $w2")
        w must equalTo(w2)
      }

      // degenerate test
      1 must equalTo(1)
    }

    "barrel-shift correctly" >> {
      val ch = CompactHilbertCurve(1, 1, 1, 1, 1, 1)

      // to the left...
      ch.barrelShiftLeft(13L, 0L) must equalTo(13L)
      ch.barrelShiftLeft(13L, 1L) must equalTo(26L)
      ch.barrelShiftLeft(13L, 2L) must equalTo(52L)
      ch.barrelShiftLeft(13L, 3L) must equalTo(41L)
      ch.barrelShiftLeft(13L, 4L) must equalTo(19L)
      ch.barrelShiftLeft(13L, 5L) must equalTo(38L)
      ch.barrelShiftLeft(13L, 6L) must equalTo(13L)

      // to the right...
      ch.barrelShiftRight(13L, 0L) must equalTo(13L)
      ch.barrelShiftRight(13L, 1L) must equalTo(38L)
      ch.barrelShiftRight(13L, 2L) must equalTo(19L)
      ch.barrelShiftRight(13L, 3L) must equalTo(41L)
      ch.barrelShiftRight(13L, 4L) must equalTo(52L)
      ch.barrelShiftRight(13L, 5L) must equalTo(26L)
      ch.barrelShiftRight(13L, 6L) must equalTo(13L)
    }

    "Gray-code rank" >> {
      def gTest(ch: CompactHilbertCurve, mu: Mask, gc: Long, iExp: Long, gcrExp: Long): Boolean = {
        val i = ch.inverseGrayCode(gc)
        val gcr = ch.grayCodeRank(mu, i)
        println(s"[Gray test] i $i ($iExp), gc $gc, gcr $gcr ($gcrExp)")

        i must equalTo(iExp)
        gcr must equalTo(gcrExp)

        // if you get this far, everything is fine
        true
      }

      "example #1" >> {
        // examples pulled from the CS-2006-07 paper
        // in which n=6, mu=010110b

        // (NB:  One correction is that the paper suggests that the gc-1(20) = 16,
        // when it appears to mean gc-1(24) = 16.)

        val ch = CompactHilbertCurve(1, 1, 1, 1, 1, 1)
        val mu = Mask(22L, 3)

        gTest(ch, mu, 8, 15, 3)
        gTest(ch, mu, 10, 12, 2)
        gTest(ch, mu, 12, 8, 0)
        gTest(ch, mu, 14, 11, 1)
        gTest(ch, mu, 24, 16, 4)
        gTest(ch, mu, 26, 19, 5)
        gTest(ch, mu, 28, 23, 7)
        gTest(ch, mu, 30, 20, 6)

        1 must equalTo(1)
      }

      "example #2" >> {
        // examples pulled from https://web.cs.dal.ca/~chamilto/hilbert/ipl.pdf
        // n=4, 2 free bits
        val ch = CompactHilbertCurve(1, 1, 1, 1)
        val mu = Mask(9L, 2)

        gTest(ch, mu, 4, 7, 1)
        gTest(ch, mu, 5, 6, 0)
        gTest(ch, mu, 12, 8, 2)
        gTest(ch, mu, 13, 9, 3)
      }
    }

    "T and T-1 should be inverses" >> {
      val ch = CompactHilbertCurve(2, 2)

      for (e <- 0 to 1; d <- 0 to 1; b <- 0 to 3) {
        val Tb = ch.T(e, d)(b)
        val b2 = ch.invT(e, d)(Tb)
        println(s"[T/T-1] T($e, $d)($b) = $Tb -> T-1($e, $d)($Tb) = $b2")
        b2 must equalTo(b)
      }

      1 must equalTo(1)
    }
  }

  def curveName = "CompactHilbertCurve"

  def createCurve(precisions: OrdinalNumber*): SpaceFillingCurve =
    CompactHilbertCurve(precisions.toOrdinalVector)

  "compact Hilbert space-filling curves" should {
    "satisfy the ordering constraints" >> {
      timeTestOrderings() must beTrue
    }

    "identify ranges from coordinate queries" >> {
      val sfc = CompactHilbertCurve(OrdinalVector(4, 4))
      val query = Query(Seq(
        OrdinalRanges(OrdinalPair(1, 5)),
        OrdinalRanges(OrdinalPair(6, 12))
      ))
      val ranges = sfc.getRangesCoveringQuery(query).toList
      ranges.zipWithIndex.foreach {
        case (range, i) =>
          println(s"[z-curve query ranges $query] $i:  $range")
      }

      ranges.size must equalTo(6)
      ranges.map(_.size).sum must equalTo(35L)
    }
  }
}
