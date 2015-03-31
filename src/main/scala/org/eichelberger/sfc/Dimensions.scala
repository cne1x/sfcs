package org.eichelberger.sfc

import org.eichelberger.sfc.SpaceFillingCurve.{Composable, OrdinalNumber}
import org.joda.time.{DateTimeZone, DateTime}
import scala.reflect._

object Dimensions {
  trait DimensionLike[T] {
    def eq(a: T, b: T): Boolean
    def lt(a: T, b: T): Boolean
    def lteq(a: T, b: T): Boolean = !gt(a, b)
    def gt(a: T, b: T): Boolean
    def gteq(a: T, b: T): Boolean = !lt(a, b)
    def compare(a: T, b: T): Int = 
      if (lt(a, b)) -1
      else {
        if (gt(a, b)) 1
        else 0
      }
    def add(a: T, b: T): T
    def subtract(a: T, b: T): T
    def multiply(a: T, b: T): T
    def divide(a: T, b: T): T
    def toDouble(a: T): Double
    def min(a: T, b: T): T =
      if (gt(a, b)) b
      else a
    def floor(a: Double): T
    def fromDouble(a: Double): T
  }

  implicit object DimensionLikeDouble extends DimensionLike[Double] {
    val maxTolerance = 1e-10
    def eq(a: Double, b: Double) = Math.abs(a - b) <= maxTolerance
    def lt(a: Double, b: Double) = (b - a) > maxTolerance
    def gt(a: Double, b: Double) = (a - b) > maxTolerance
    def add(a: Double, b: Double) = a + b
    def subtract(a: Double, b: Double) = a - b
    def multiply(a: Double, b: Double) = a * b
    def divide(a: Double, b: Double) = a / b
    def toDouble(a: Double) = a
    def floor(a: Double) = Math.floor(a)
    def fromDouble(a: Double) = a
  }

  implicit object DimensionLikeLong extends DimensionLike[Long] {
    def eq(a: Long, b: Long) = a == b
    def lt(a: Long, b: Long) = a < b
    def gt(a: Long, b: Long) = a > b
    def add(a: Long, b: Long) = a + b
    def subtract(a: Long, b: Long) = a - b
    def multiply(a: Long, b: Long) = a * b
    def divide(a: Long, b: Long) = a / b
    def toDouble(a: Long) = a.toDouble
    def floor(a: Double) = Math.floor(a).toLong
    def fromDouble(a: Double) = Math.round(a).toLong
  }

  implicit object DimensionLikeDate extends DimensionLike[DateTime] {
    def eq(a: DateTime, b: DateTime) = a == b
    def lt(a: DateTime, b: DateTime) = a.getMillis < b.getMillis
    def gt(a: DateTime, b: DateTime) = a.getMillis > b.getMillis
    def add(a: DateTime, b: DateTime) = new DateTime(a.getMillis + b.getMillis, DateTimeZone.forID("UTC"))
    def subtract(a: DateTime, b: DateTime) = new DateTime(a.getMillis - b.getMillis, DateTimeZone.forID("UTC"))
    def multiply(a: DateTime, b: DateTime) = new DateTime(a.getMillis * b.getMillis, DateTimeZone.forID("UTC"))
    def divide(a: DateTime, b: DateTime) = new DateTime(a.getMillis / b.getMillis, DateTimeZone.forID("UTC"))
    def toDouble(a: DateTime) = a.getMillis.toDouble
    def floor(a: Double) = new DateTime(Math.floor(a).toLong, DateTimeZone.forID("UTC"))
    def fromDouble(a: Double) = new DateTime(a.toLong, DateTimeZone.forID("UTC"))
  }
}

import Dimensions._

case class HalfDimension[T](extreme: T, isExtremeIncluded: Boolean)

// for now, assume all dimensions are bounded
// (reasonable, unless you're really going to use BigInt or
// some other arbitrary-precision class as the basis)
case class Dimension[T : DimensionLike](name: String, min: T, isMinIncluded: Boolean, max: T, isMaxIncluded: Boolean, precision: OrdinalNumber)(implicit classTag: ClassTag[T])
  extends Composable {

  val basis = implicitly[DimensionLike[T]]

  val span = basis.subtract(max, min)
  val numBins = 1L << precision
  val penultimateBin = numBins - 1L

  val doubleSpan = basis.toDouble(span)
  val doubleNumBins = numBins.toDouble
  val doubleMin = basis.toDouble(min)
  val doubleMax = basis.toDouble(max)

  def containsAny(value: Any): Boolean = {
    val coercedValue: T = value.asInstanceOf[T]
    contains(coercedValue)
  }

  def contains(value: T): Boolean =
    (basis.gt(value, min) ||(basis.eq(value, min) && isMinIncluded)) &&
      (basis.lt(value, max) ||(basis.eq(value, max) && isMaxIncluded))

  def indexAny(value: Any): OrdinalNumber = {
    val coercedValue: T = value.asInstanceOf[T]
    index(coercedValue)
  }

  def index(value: T): OrdinalNumber = {
    val doubleValue = basis.toDouble(value)
    Math.min(
      Math.floor(doubleNumBins * (doubleValue - doubleMin) / doubleSpan),
      penultimateBin
    ).toLong
  }

  def getLowerBound(ordinal: OrdinalNumber): HalfDimension[T] = {
    /*
    val minimum = numeric.toDouble(dim.min) + numeric.toDouble(dim.span) * ordinal.toDouble / size.toDouble
    val incMin = dim.isMinIncluded || (ordinal == 0L)
     */
    val x = basis.fromDouble(doubleMin + doubleSpan * ordinal.toDouble / doubleNumBins)
    val included = isMinIncluded || (ordinal == 0L)
    HalfDimension[T](x, included)
  }

  def getUpperBound(ordinal: OrdinalNumber): HalfDimension[T] = {
    /*
    val max = numeric.toDouble(dim.min) + numeric.toDouble(dim.span) * (1L + ordinal.toDouble) / size.toDouble
    val incMax = dim.isMaxIncluded && (ordinal == (size - 1L))
     */
    val x = basis.fromDouble(doubleMin + doubleSpan * (1L + ordinal.toDouble) / doubleNumBins)
    val included = isMinIncluded || (ordinal == 0L)
    HalfDimension[T](x, included)
  }

  def inverseIndex(ordinal: OrdinalNumber): Dimension[T] = {
    val lowerBound = getLowerBound(ordinal)
    val upperBound = getUpperBound(ordinal)
    new Dimension[T](name, lowerBound.extreme, lowerBound.isExtremeIncluded, upperBound.extreme, upperBound.isExtremeIncluded, 1L)
  }

  override def toString: String =
    (if (isMinIncluded) "[" else "(") +
      min.toString + ", " + max.toString +
      (if (isMaxIncluded) "]" else ")") +
      "@" + precision.toString
}

object DefaultDimensions {
  import Dimensions._

  val dimLongitude = Dimension[Double]("x", -180.0, isMinIncluded = true, +180.0, isMaxIncluded = true, 18L)
  val dimLatitude = Dimension[Double]("y", -90.0, isMinIncluded = true, +90.0, isMaxIncluded = true, 17L)

  val MinDate = new DateTime(1900,  1,  1,  0,  0,  0, DateTimeZone.forID("UTC"))
  val MaxDate = new DateTime(2100, 12, 31, 23, 59, 59, DateTimeZone.forID("UTC"))
  val dimTime = Dimension("t", MinDate, isMinIncluded = true, MaxDate, isMaxIncluded = true, 15L)

  def createLongitude(atPrecision: OrdinalNumber): Dimension[Double] =
    dimLongitude.copy(precision = atPrecision)

  def createLatitude(atPrecision: OrdinalNumber): Dimension[Double] =
    dimLatitude.copy(precision = atPrecision)

  def createDateTime(atPrecision: OrdinalNumber): Dimension[DateTime] =
    dimTime.copy(precision = atPrecision)

  def createDateTime(minDate: DateTime, maxDate: DateTime, atPrecision: OrdinalNumber): Dimension[DateTime] =
    dimTime.copy(min = minDate, max = maxDate, precision = atPrecision)

  def createDimension[T](name: String, minimum: T, maximum: T, precision: OrdinalNumber)(implicit dimLike: DimensionLike[T], ctag: ClassTag[T]): Dimension[T] =
    Dimension[T](name, minimum, isMinIncluded = true, maximum, isMaxIncluded = true, precision)
}
