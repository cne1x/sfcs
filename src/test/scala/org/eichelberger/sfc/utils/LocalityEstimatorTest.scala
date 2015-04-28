package org.eichelberger.sfc.utils

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.{CompactHilbertCurve, RowMajorCurve, ZCurve}
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LocalityEstimatorTest extends Specification with LazyLogging {
  sequential

  "locality" should {
    "evaluate on square 2D curves" >> {
      (1 to 6).foreach { p =>
        val locR = LocalityEstimator(RowMajorCurve(p, p)).locality
        println(s"[LOCALITY R($p, $p)] $locR")

        val locZ = LocalityEstimator(ZCurve(p, p)).locality
        println(s"[LOCALITY Z($p, $p)] $locZ")

        val locH = LocalityEstimator(CompactHilbertCurve(p, p)).locality
        println(s"[LOCALITY H($p, $p)] $locH")
      }

      1 must beEqualTo(1)
    }

    "evaluate on non-square 2D curves" >> {
      (1 to 6).foreach { p =>
        val locR = LocalityEstimator(RowMajorCurve(p << 1L, p)).locality
        println(s"[LOCALITY R(${p*2}, $p)] $locR")

        val locZ = LocalityEstimator(ZCurve(p << 1L, p)).locality
        println(s"[LOCALITY Z(${p*2}, $p)] $locZ")

        val locH = LocalityEstimator(CompactHilbertCurve(p << 1L, p)).locality
        println(s"[LOCALITY H(${p*2}, $p)] $locH")
      }

      1 must beEqualTo(1)
    }
  }
}
