package org.eichelberger.sfc.utils

import java.io.{BufferedOutputStream, FileOutputStream, PrintStream}

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.SpaceFillingCurve.{OrdinalVector, _}
import org.eichelberger.sfc._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

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

    "be able to dump a 2D rowmajor curve to screen" >> {
      val sfc = new RowMajorCurve(OrdinalVector(4, 4)) with RenderSource {
        def getCurveName = "Row-major"
      }
      val screenTarget = new ScreenRenderTarget
      sfc.render(screenTarget)

      1 must equalTo(1)
    }

    "be able to dump a 3D Z-curve to screen" >> {
      val sfc = new RowMajorCurve(OrdinalVector(3, 3, 3)) with RenderSource {
        def getCurveName = "Row-major"
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
        override val drawNumbers = true
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

      new CompactHilbertCurve(OrdinalVector(2, 2, 2)) with RenderSource { def getCurveName = "Compact Hilbert" }.render(dotTarget("h(2,2,2)", 8))

      new ZCurve(OrdinalVector(4, 4)) with RenderSource { def getCurveName = "Z" }.render(dotTarget("z(4,4)", 8))
      new CompactHilbertCurve(OrdinalVector(4, 4)) with RenderSource { def getCurveName = "Compact Hilbert" }.render(dotTarget("h(4,4)", 8))
      new RowMajorCurve(OrdinalVector(4, 4)) with RenderSource { def getCurveName = "Row-major" }.render(dotTarget("r(4,4)", 8))

      new ZCurve(OrdinalVector(3, 5)) with RenderSource { def getCurveName = "Z" }.render(dotTarget("z(3,5)", 8))
      new CompactHilbertCurve(OrdinalVector(3, 5)) with RenderSource { def getCurveName = "Compact Hilbert" }.render(dotTarget("h(3,5)", 8))
      new RowMajorCurve(OrdinalVector(3, 5)) with RenderSource { def getCurveName = "Row-major" }.render(dotTarget("r(3,5)", 8))

      1 must equalTo(1)
    }

    "be able to render small curves for POV-Ray" >> {
      def povTarget(fileName: String) = new PovrayRenderTarget() {
        override val pw: PrintStream =
          new java.io.PrintStream(new BufferedOutputStream(new FileOutputStream(s"/tmp/$fileName.inc")))
        override def afterRendering(sfc: RenderSource): Unit = {
          super.afterRendering(sfc)
          pw.close()
        }
      }

      // square cubes
      new RowMajorCurve(OrdinalVector(4, 4, 4)) with RenderSource { override val useSlices = false; def getCurveName = "R444" }.render(povTarget("r(4,4,4)"))
      new ZCurve(OrdinalVector(4, 4, 4)) with RenderSource { override val useSlices = false; def getCurveName = "Z444" }.render(povTarget("z(4,4,4)"))
      new CompactHilbertCurve(OrdinalVector(4, 4, 4)) with RenderSource { override val useSlices = false; def getCurveName = "H444" }.render(povTarget("h(4,4,4)"))

      // oblong cubes
      new RowMajorCurve(OrdinalVector(4, 4, 5)) with RenderSource { override val useSlices = false; def getCurveName = "R445" }.render(povTarget("r(4,4,5)"))
      new ZCurve(OrdinalVector(4, 4, 5)) with RenderSource { override val useSlices = false; def getCurveName = "Z445" }.render(povTarget("z(4,4,5)"))
      new CompactHilbertCurve(OrdinalVector(4, 4, 5)) with RenderSource { override val useSlices = false; def getCurveName = "H445" }.render(povTarget("h(4,4,5)"))

      1 must equalTo(1)
    }
  }
}
