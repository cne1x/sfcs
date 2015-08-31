package org.eichelberger.sfc.utils

import com.typesafe.scalalogging.Logging
import org.eichelberger.sfc.SpaceFillingCurve.{Composable, OrdinalVector, OrdinalNumber, SpaceFillingCurve}
import org.eichelberger.sfc._
import scala.util.parsing.combinator.RegexParsers

/*
 * Examples:
 * - R(2,3)
 * - R(2,3,4)
 * - H(2, Z(7))
 * - Z(R(4,H(2,2)),8)
 */
object CompositionParser extends RegexParsers {
  val LPAREN = "("
  val RPAREN = ")"
  val COMMA = ","

  val R_CURVE_NAME = """(?i)r""".r
  val Z_CURVE_NAME = """(?i)z""".r
  val H_CURVE_NAME = """(?i)h""".r

  def curveName: Parser[String] = R_CURVE_NAME | Z_CURVE_NAME | H_CURVE_NAME ^^ { _.toString }

  def precision: Parser[Int] = """\d+""".r ^^ { _.toInt }

  case class PrecisionOrCurve(precisionOpt: Option[Int], curveOpt: Option[SpaceFillingCurve])

  def childArg: Parser[PrecisionOrCurve] = (precision | curveParser) ^^ {
    case p: Int               => PrecisionOrCurve(Some(p), None)
    case c: SpaceFillingCurve => PrecisionOrCurve(None, Some(c))
  }

  def curveParser: Parser[ComposedCurve] = curveName ~ LPAREN ~ repsep(childArg, COMMA) ~ RPAREN ^^ {
    case name ~ lp ~ children ~ rp =>
      val precisions = OrdinalVector(children.flatMap {
        case PrecisionOrCurve(Some(p), None) => Seq(p.toLong)
        case PrecisionOrCurve(None, Some(c)) => c.precisions.toSeq
      }:_*)
      val curve = name match {
        case s: String if s.matches(R_CURVE_NAME.toString()) => new RowMajorCurve(precisions)
        case s: String if s.matches(Z_CURVE_NAME.toString()) => new ZCurve(precisions)
        case s: String if s.matches(H_CURVE_NAME.toString()) => new CompactHilbertCurve(precisions)
      }
      val childParams: Seq[Composable] = children.map {
        case PrecisionOrCurve(Some(p), None) => DefaultDimensions.createIdentityDimension(p)
        case PrecisionOrCurve(None, Some(c)) => c
      }
      new ComposedCurve(curve, childParams)
    }

  def buildWholeNumberCurve(s: String): ComposedCurve = parse(curveParser, s).get
}

case class CompositionParserException(msg: String) extends Exception(msg)