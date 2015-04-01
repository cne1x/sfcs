package org.eichelberger.sfc.utils

import java.io.{FileOutputStream, BufferedOutputStream, PrintStream}

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

    "be able to render small curves for Graphviz" >> {
      def dotTarget(fileName: String, precision: OrdinalNumber) = new GraphvizRenderTarget() {
        val hue: Float = (39.0 / 255.0).toFloat
        override val pw: PrintStream =
          new java.io.PrintStream(new BufferedOutputStream(new FileOutputStream(s"/tmp/$fileName.dot")))
        override val drawNumbers = false
        override val drawArrows = true
        override val cellShadingRamp = Option(ShadeRamp(
          new ShadeRampEndpoint(0L, hue, 0.0f, 0.1f),
          new ShadeRampEndpoint(1L << precision, hue, 0.0f, 0.8f)
        ))
        override def afterRendering(sfc: RenderSource): Unit = {
          super.afterRendering(sfc)
          pw.close()
        }
      }

      new ZCurve(OrdinalVector(4, 4)) with RenderSource { def getCurveName = "Z" }.render(dotTarget("z(4,4)", 8))
      new CompactHilbertCurve(OrdinalVector(4, 4)) with RenderSource { def getCurveName = "Compact Hilbert" }.render(dotTarget("h(4,4)", 8))
      new RectilinearCurve(OrdinalVector(4, 4)) with RenderSource { def getCurveName = "Rectilinear" }.render(dotTarget("r(4,4)", 8))

      new ZCurve(OrdinalVector(3, 5)) with RenderSource { def getCurveName = "Z" }.render(dotTarget("z(3,5)", 8))
      new CompactHilbertCurve(OrdinalVector(3, 5)) with RenderSource { def getCurveName = "Compact Hilbert" }.render(dotTarget("h(3,5)", 8))
      new RectilinearCurve(OrdinalVector(3, 5)) with RenderSource { def getCurveName = "Rectilinear" }.render(dotTarget("r(3,5)", 8))

      1 must equalTo(1)
    }
  }
}
