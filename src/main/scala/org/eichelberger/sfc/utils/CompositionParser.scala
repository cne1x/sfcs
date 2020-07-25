package org.eichelberger.sfc.utils

import com.typesafe.scalalogging.Logging
import org.eichelberger.sfc.SpaceFillingCurve.{Composable, OrdinalNumber, OrdinalVector, SpaceFillingCurve}
import org.eichelberger.sfc._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.util.parsing.combinator.RegexParsers

/*
 * Examples:
 * - R(2,3)
 * - R(2,3,4)
 * - H(2, Z(7))
 * - Z(R(4,H(2,2)),8)
 *
 * Examples:
 *   R(t(15), Z(x(10), y(5)))
 *   R(Z(x(10), y(5)), t(15))
 */
object CompositionParser extends RegexParsers {
  val dtf = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")

  val LPAREN = """\(""".r
  val RPAREN = """\)""".r
  val COMMA = ","

  val R_CURVE_NAME = """R""".r
  val Z_CURVE_NAME = """Z""".r
  val H_CURVE_NAME = """H""".r

  def intLiteral: Parser[Int] = """\d+""".r ^^ { _.toInt }

  def doubleLiteral: Parser[Double] = """[+\-]?[0-9]*\.?[0-9]+""".r ^^ { _.toDouble }

  def dateLiteral: Parser[DateTime] = """\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d\.\d\d\dZ""".r ^^ { s => dtf.parseDateTime(s) }

  def longitude: Parser[Dimension[Double]] = """x""".r ~> LPAREN ~> intLiteral ~ opt(COMMA ~ doubleLiteral ~ COMMA ~ doubleLiteral) <~ RPAREN ^^ {
    case p ~ None => DefaultDimensions.createLongitude(p.toInt)
    case p ~ Some(_ ~ min ~ _ ~ max) =>
      Dimension("x", min, isMinIncluded = true, max, isMaxIncluded = true, p.toLong)
  }

  def latitude: Parser[Dimension[Double]] = """y""".r ~> LPAREN ~> intLiteral ~ opt(COMMA ~ doubleLiteral ~ COMMA ~ doubleLiteral) <~ RPAREN ^^ {
    case p ~ None => DefaultDimensions.createLatitude(p.toInt)
    case p ~ Some(_ ~ min ~ _ ~ max) =>
      Dimension("y", min, isMinIncluded = true, max, isMaxIncluded = true, p.toLong)
  }

  case class Bounds(min: String, max: String)

  //Dimension("t", MinDate, isMinIncluded = true, MaxDate, isMaxIncluded = true, 60L)
  def dateTime: Parser[Dimension[DateTime]] = """t""".r ~> LPAREN ~> intLiteral ~ opt(COMMA ~ dateLiteral ~ COMMA ~ dateLiteral) <~ RPAREN ^^ {
    case p ~ None => DefaultDimensions.createDateTime(p.toInt)
    case p ~ Some(_ ~ minDate ~ _ ~ maxDate) =>
      Dimension("t", minDate, isMinIncluded = true, maxDate, isMaxIncluded = true, p.toLong)
  }

  def longDimName: Parser[String] = "u".r | "v".r | "w".r

  def longDim: Parser[Dimension[Long]] = longDimName ~ LPAREN ~ intLiteral ~ opt(COMMA ~ doubleLiteral ~ COMMA ~ doubleLiteral) <~ RPAREN ^^ {
    case name ~ _ ~ p ~ None =>
      Dimension[Long](name, 0L, isMinIncluded=true, (1L << p.toLong) - 1L, isMaxIncluded=true, p.toLong)
    case name ~ _ ~ p ~ Some(_ ~ min ~ _ ~ max) =>
      Dimension[Long](name, min.toLong, isMinIncluded=true, max.toLong, isMaxIncluded=true, p.toLong)
  }

  def dimension: Parser[Dimension[_]] = longitude | latitude | dateTime | longDim

  def curveName: Parser[String] = R_CURVE_NAME | Z_CURVE_NAME | H_CURVE_NAME ^^ { _.toString }

  def precision: Parser[Dimension[Long]] = intLiteral ^^ { p => DefaultDimensions.createIdentityDimension(p) }

  def childArg: Parser[Composable] = dimension | precision | curveParser

  def curveParser: Parser[ComposedCurve] = curveName ~ LPAREN ~ repsep(childArg, COMMA) ~ RPAREN ^^ {
    case name ~ _ ~ children ~ _ =>
      val precisions = OrdinalVector(children.map {
        case c: SpaceFillingCurve => c.precisions.sum
        case d: Dimension[_] => d.precision
      }:_*)
      val curve = name match {
        case s: String if s.matches(R_CURVE_NAME.toString()) => new RowMajorCurve(precisions)
        case s: String if s.matches(Z_CURVE_NAME.toString()) => new ZCurve(precisions)
        case s: String if s.matches(H_CURVE_NAME.toString()) => new CompactHilbertCurve(precisions)
      }
      new ComposedCurve(curve, children)
    }

  def buildWholeNumberCurve(s: String): ComposedCurve = parse(curveParser, s).get
}

case class CompositionParserException(msg: String) extends Exception(msg)
