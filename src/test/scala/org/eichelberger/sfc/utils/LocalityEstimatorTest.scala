package org.eichelberger.sfc.utils

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.SpaceFillingCurve.SpaceFillingCurve
import org.eichelberger.sfc.{RowMajorCurve, CompactHilbertCurve, ZCurve}
import org.eichelberger.sfc.utils.LocalityEstimator.SampleItem
import org.junit.Ignore
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.specs2.mutable.Specification

@RunWith(classOf[JUnitRunner])
class LocalityEstimatorTest extends Specification with LazyLogging {
  sequential

  "covariance" should {
    val locEst = LocalityEstimator(null)

    "produce proper values in isolation #1" >> {
      val sample = (1 to 9).map(i => SampleItem(i.toDouble, 9.0 - i.toDouble))
      locEst.covariance(sample) must beCloseTo(-6.6667, 0.0001)
    }

    "produce proper values in isolation #2" >> {
      val sample = (1 to 9).map(i => SampleItem(i.toDouble, i.toDouble))
      locEst.covariance(sample) must beCloseTo(6.6667, 0.0001)
    }

    "produce proper values in isolation #3" >> {
      val x = (1 to 9).map(_.toDouble)
      val y = Seq(0.865606416,0.2333345101, 0.9589602181, 0.6056712451, 0.5999978613, 0.6725118309, 0.8656152433, 0.802448394, 0.2022216029)
      val sample = x.zip(y).map { case (xi, yi) => SampleItem(xi, yi) }
      locEst.covariance(sample) must beCloseTo(-0.1184496627, 0.0001)
    }
  }

  "study" should {
    "yield 2D curve locality scaling" >> pending {
      val pw = new java.io.PrintWriter(new java.io.BufferedWriter(new java.io.FileWriter("/tmp/locality-2d.tsv")))

      def test(curve: SpaceFillingCurve, px: Int, py: Int): Unit = {
        val loc = LocalityEstimator(curve).locality
        println(s"[2D LOCALITY ($px, $py)] ${curve.name}:  $loc")
        pw.println(s"${curve.name}\t$px\t$py\t${loc.locality}\t${loc.coverage}")
      }

      for (px <- 1 to 10; py <- 1 to 10) {
        test(RowMajorCurve(px, py), px, py)
        test(ZCurve(px, py), px, py)
        test(CompactHilbertCurve(px, py), px, py)
      }

      pw.close()
      Thread.sleep(1000L * 10L)

      1 must equalTo(1)
    }
  }
}
