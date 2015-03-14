package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.specs2.mutable.Specification
import SpaceFillingCurve._

@RunWith(classOf[JUnitRunner])
class LexicographicTest extends Specification with LazyLogging {
  sequential
  
  "Lexicographical encoding" should {
    val precisions = new ords2ordvec(Seq(18L, 17L)).toOrdinalVector

    val sfc = ZCurve(precisions)

    val Longitude = Real1DPartitioner(RealDimension(-180.0, true, 180.0, true), 1L << 18L)
    val Latitude = Real1DPartitioner(RealDimension(-90.0, true, 90.0, true), 1L << 17L)

    "work for a known point" >> {
      val x = -78.488407
      val y = 38.038668

      val point = OrdinalVector(Longitude.index(x), Latitude.index(y))
      val idx = sfc.index(point)
      val gh = sfc.lexEncodeIndex(idx)

      gh must equalTo("dqb0muw")
    }

    "be consistent round-trip" >> {
      val xs = (-180.0 to 180.0 by 33.3333).toSeq ++ Seq(180.0)
      val ys = (-90.0 to 90.0 by 33.3333).toSeq ++ Seq(90.0)
      for (x <- xs; y <- ys) {
        val ix = Longitude.index(x)
        val iy = Latitude.index(y)
        val point = OrdinalVector(ix, iy)
        val idx = sfc.index(point)
        val gh = sfc.lexEncodeIndex(idx)
        val idx2 = sfc.lexDecodeIndex(gh)
        idx2 must equalTo(idx)
        val point2 = sfc.inverseIndex(idx2)
        point2(0) must equalTo(ix)
        point2(1) must equalTo(iy)
        val rx = Longitude.inverseIndex(ix)
        val ry = Latitude.inverseIndex(iy)

        val sx = x.formatted("%8.3f")
        val sy = y.formatted("%8.3f")
        val sidx = idx.formatted("%20d")
        val sxmin = rx.min.formatted("%9.4f")
        val sxmax = rx.max.formatted("%9.4f")
        val symin = ry.min.formatted("%9.4f")
        val symax = ry.max.formatted("%9.4f")
        println(s"[LEXI ROUND-TRIP] POINT($sx $sy) -> $sidx = $gh -> ($rx, $ry)")
      }

      // degenerate
      1 must equalTo(1)
    }
  }

  "multiple lexicographical encoders" should {
    "return different results for different base resolutions" >> {
      val x = -78.488407
      val y = 38.038668

      for (xBits <- 1 to 30; yBits <- xBits - 1 to xBits if yBits > 0) {
        val precisions = new ords2ordvec(Seq(xBits, yBits)).toOrdinalVector
        val sfc = ZCurve(precisions)

        val Longitude = Real1DPartitioner(RealDimension(-180.0, true, 180.0, true), 1L << xBits)
        val Latitude = Real1DPartitioner(RealDimension(-90.0, true, 90.0, true), 1L << yBits)

        val idx = sfc.index(OrdinalVector(Longitude.index(x), Latitude.index(y)))
        val gh = sfc.lexEncodeIndex(idx)
        val idx2 = sfc.lexDecodeIndex(gh)

        idx2 must equalTo(idx)

        println(s"[LEXI ACROSS RESOLUTIONS] mx $xBits + my $yBits = base ${sfc.alphabet.size}, idx $idx -> gh $gh -> $idx2")
      }

      // degenerate
      1 must equalTo(1)
    }
  }
}
