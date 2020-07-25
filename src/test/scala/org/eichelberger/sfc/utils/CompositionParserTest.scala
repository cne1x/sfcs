package org.eichelberger.sfc.utils

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.DefaultDimensions.IdentityDimension
import org.eichelberger.sfc.SpaceFillingCurve.{OrdinalVector, SpaceFillingCurve}
import org.eichelberger.sfc._
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CompositionParserTest extends Specification {
  sequential

  def parsableCurve(curve: SpaceFillingCurve): String = curve match {
    case c: ComposedCurve =>
      c.delegate.name.charAt(0).toString + c.children.map {
        case d: IdentityDimension      => d.precision
        case d: Dimension[DateTime] if d.min.isInstanceOf[DateTime] => s"t(${d.precision})"
        case d: Dimension[Double] if d.doubleMin < -100 => s"x(${d.precision})"
        case d: Dimension[Double] if d.doubleMin > -100 => s"y(${d.precision})"
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
    println(s"[CURVE PARSER FORWARD]\n  Input:    $toParse\n  Output:  '$fromParse''")
    toParse == fromParse
  }

  def eval(toParse: String): Boolean = {
    val parsed: ComposedCurve = CompositionParser.buildWholeNumberCurve(toParse)
    val fromParse: String = parsableCurve(parsed)
    println(s"[CURVE PARSER BACKWARD]\n  Input:  '$toParse'\n  Output:  $fromParse")
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

      1 must_== 1
    }
  }

  "curves with explicit dimensions" should {

    "parse without explicit bounds correctly" >> {
      eval("Z(x(10))") must beTrue
      eval("Z(x(10), y(5))") must beTrue
      eval("Z(y(5), x(10))") must beTrue
      eval("Z(t(15), H(y(5), x(10)))") must beTrue
      eval("Z(H(y(5), x(10)), t(15))") must beTrue

      1 must_== 1
    }

    "parse with explicit TIME bounds correctly" >> {
      val cADefault = CompositionParser.buildWholeNumberCurve("Z(t(10))")
      val cTDefault: Dimension[DateTime] = cADefault.children.head.asInstanceOf[Dimension[DateTime]]
      cTDefault.min must_== DefaultDimensions.MinDate
      cTDefault.max must_== DefaultDimensions.MaxDate

      val dtMin = "1990-01-01T00:00:00.000Z"
      val dtMax = "2050-12-31T23:59:59.999Z"
      val cACustom = CompositionParser.buildWholeNumberCurve(s"Z(t(10, $dtMin, $dtMax))")
      val cTCustom: Dimension[DateTime] = cACustom.children.head.asInstanceOf[Dimension[DateTime]]
      cTCustom.min must_== CompositionParser.dtf.parseDateTime(dtMin)
      cTCustom.max must_== CompositionParser.dtf.parseDateTime(dtMax)

      1 must_== 1
    }

    "parse with explicit LONGITUDE bounds correctly" >> {
      val cADefault = CompositionParser.buildWholeNumberCurve("Z(x(10))")
      val cXDefault: Dimension[Double] = cADefault.children.head.asInstanceOf[Dimension[Double]]
      cXDefault.min must_== DefaultDimensions.dimLongitude.min
      cXDefault.max must_== DefaultDimensions.dimLongitude.max

      val dxMin = "-39.876"
      val dxMax = "41.234"
      val cACustom = CompositionParser.buildWholeNumberCurve(s"Z(x(10, $dxMin, $dxMax))")
      val cXCustom: Dimension[Double] = cACustom.children.head.asInstanceOf[Dimension[Double]]
      cXCustom.min must_== dxMin.toDouble
      cXCustom.max must_== dxMax.toDouble

      1 must_== 1
    }

    "parse with explicit secondary indexes (U, V, W) set correctly" >> {
      // ... when using implicit per-dimension bounds
      val cImplicit = CompositionParser.buildWholeNumberCurve("Z(u(10), v(10))")
      cImplicit.cardinalities must_== List(1024, 1024)
      cImplicit.children.head.asInstanceOf[Dimension[Long]].min must_== 0
      cImplicit.children.head.asInstanceOf[Dimension[Long]].max must_== 1023
      cImplicit.children.last.asInstanceOf[Dimension[Long]].min must_== 0
      cImplicit.children.last.asInstanceOf[Dimension[Long]].max must_== 1023

      // ... when using explicit per-dimension bounds
      val cExplicit = CompositionParser.buildWholeNumberCurve("Z(u(10, 4, 12), v(10, 1, 18))")
      cExplicit.cardinalities must_== List(1024, 1024)
      cExplicit.children.head.asInstanceOf[Dimension[Long]].min must_== 4
      cExplicit.children.head.asInstanceOf[Dimension[Long]].max must_== 12
      cExplicit.children.last.asInstanceOf[Dimension[Long]].min must_== 1
      cExplicit.children.last.asInstanceOf[Dimension[Long]].max must_== 18

      1 must_== 1
    }

    "index points correctly" >> {
      val dtMin = "1990-01-01T00:00:00.000Z"
      val dtMax = "2050-12-31T23:59:59.999Z"
      val curve = CompositionParser.buildWholeNumberCurve(s"Z(t(10, $dtMin, $dtMax), H(x(8), y(7)))")

      println(s"curve:  ${curve.name}")
      println(s"top precisions:  ${curve.precisions.toSeq.map(_.toString).mkString("[", ",", "]")}")

      val index = curve.pointToIndex(Seq[Any](
        CompositionParser.dtf.parseDateTime("2020-07-10T12:23:31.000Z"),
        -79.9,
        38.2
      ))
      println(s"index:  $index")

      val cell = curve.indexToCell(index)
      println(s"cell:  $cell")

      (index >= 0L) must beTrue
      (index < (1L << 25)) must beTrue

      1 must_== 1
    }
  }
}
