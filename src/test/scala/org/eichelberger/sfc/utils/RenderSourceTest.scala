package org.eichelberger.sfc.utils

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.{RectilinearCurve, CompactHilbertCurve, ZCurve}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.specs2.mutable.Specification

@RunWith(classOf[JUnitRunner])
class RenderSourceTest extends Specification with LazyLogging {
  sequential

  "to-screen renderers" should {
    "be able to dump a 2D Compact-Hilbert-curve to screen" >> {
      val sfc = new CompactHilbertCurve(OrdinalVector(4, 4)) with RenderSource {
        def getCurveName = "Compact-Hilbert"
      }
      val screenTarget = new ScreenRenderTarget
      sfc.render(screenTarget)

      1 must equalTo(1)
    }

    "be able to dump a 3D Compact-Hilbert-curve to screen" >> {
      val sfc = new CompactHilbertCurve(OrdinalVector(3, 3, 3)) with RenderSource {
        def getCurveName = "Compact-Hilbert"
      }
      val screenTarget = new ScreenRenderTarget
      sfc.render(screenTarget)

      1 must equalTo(1)
    }

    "be able to dump a 2D Z-curve to screen" >> {
      val sfc = new ZCurve(OrdinalVector(4, 4)) with RenderSource {
        def getCurveName = "Z"
      }
      val screenTarget = new ScreenRenderTarget
      sfc.render(screenTarget)

      1 must equalTo(1)
    }

    "be able to dump a 3D Z-curve to screen" >> {
      val sfc = new ZCurve(OrdinalVector(3, 3, 3)) with RenderSource {
        def getCurveName = "Z"
      }
      val screenTarget = new ScreenRenderTarget
      sfc.render(screenTarget)

      1 must equalTo(1)
    }

    "be able to dump a 2D rectilinear curve to screen" >> {
      val sfc = new RectilinearCurve(OrdinalVector(4, 4)) with RenderSource {
        def getCurveName = "Rectilinear"
      }
      val screenTarget = new ScreenRenderTarget
      sfc.render(screenTarget)

      1 must equalTo(1)
    }

    "be able to dump a 3D Z-curve to screen" >> {
      val sfc = new RectilinearCurve(OrdinalVector(3, 3, 3)) with RenderSource {
        def getCurveName = "Rectilinear"
      }
      val screenTarget = new ScreenRenderTarget
      sfc.render(screenTarget)

      1 must equalTo(1)
    }

    "be able to render three small curves for Graphviz" >> {
      val dotTarget = new GraphvizRenderTarget()

      new ZCurve(OrdinalVector(2, 2)) with RenderSource { def getCurveName = "Z" }.render(dotTarget)
      new CompactHilbertCurve(OrdinalVector(2, 2)) with RenderSource { def getCurveName = "Compact Hilbert" }.render(dotTarget)
      new RectilinearCurve(OrdinalVector(2, 2)) with RenderSource { def getCurveName = "Rectilinear" }.render(dotTarget)

      1 must equalTo(1)
    }
  }
}
