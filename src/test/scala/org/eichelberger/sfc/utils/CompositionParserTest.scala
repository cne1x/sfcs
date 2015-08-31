package org.eichelberger.sfc.utils

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.SpaceFillingCurve.SpaceFillingCurve
import org.eichelberger.sfc.SpaceFillingCurve.SpaceFillingCurve
import org.eichelberger.sfc._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CompositionParserTest extends Specification {
  sequential

  def parsableCurve(curve: SpaceFillingCurve): String = curve match {
    case c: ComposedCurve =>
      c.delegate.name.charAt(0).toString + c.children.map {
        case d: Dimension[_]      => d.precision
        case s: SubDimension[_]   => s.precision
        case c: SpaceFillingCurve => parsableCurve(c)
      }.mkString("(", ", ", ")")
    case s =>
      s.name.charAt(0).toString + s.precisions.toSeq.map(_.toString).mkString("(", ", ", ")")
  }

  def eval(curve: ComposedCurve): Boolean = {
    val toParse: String = parsableCurve(curve)
    val parsed: ComposedCurve = CompositionParser.buildWholeNumberCurve(toParse)
    val fromParse: String = parsableCurve(parsed)
    println(s"[CURVE PARSER]\n  Input:  $toParse\n  Output:  $fromParse")
    toParse == fromParse
  }

  "simple expressions" should {
    val R23 = new ComposedCurve(
      RowMajorCurve(2, 3),
      Seq(
        DefaultDimensions.createIdentityDimension(2),
        DefaultDimensions.createIdentityDimension(3)
      )
    )
    val H_2_R23 = new ComposedCurve(
      CompactHilbertCurve(2),
      Seq(
        DefaultDimensions.createIdentityDimension(2),
        R23
      )
    )
    val Z_R23_2 = new ComposedCurve(
      ZCurve(2),
      Seq(
        R23,
        DefaultDimensions.createIdentityDimension(2)
      )
    )

    "parse correctly" >> {
      eval(R23) must beTrue
      eval(H_2_R23) must beTrue
      eval(Z_R23_2) must beTrue
    }
  }
}
