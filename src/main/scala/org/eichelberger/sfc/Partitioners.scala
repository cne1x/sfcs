package org.eichelberger.sfc

import org.eichelberger.sfc.SpaceFillingCurve.OrdinalNumber

trait Dimension[T] {
  def min: T
  def isMinIncluded: Boolean
  def max: T
  def isMaxIncluded: Boolean
  override def toString: String =
    (if (isMinIncluded) "[" else "(") +
      min.toString + ", " + max.toString +
      (if (isMaxIncluded) "]" else ")")
}

case class RealDimension(min: Double, isMinIncluded: Boolean, max: Double, isMaxIncluded: Boolean) extends Dimension[Double] {
  require(min <= max, s"Minimum ($min) must not be greater than maximum ($max)")

  val span = max - min
}

trait Partitioner[T <: Dimension[_]] {
  type Point
  def size: Long
  def index(point: Point): OrdinalNumber
  def inverseIndex(ordinal: OrdinalNumber): T
}

case class Real1DPartitioner(dim: RealDimension, size: Long) extends Partitioner[RealDimension] {
  type Point = Double

  def index(point: Point): OrdinalNumber = {
    Math.min(Math.floor(size.toDouble * (point - dim.min) / dim.span), size - 1L).toLong
  }

  def inverseIndex(ordinal: OrdinalNumber): RealDimension = {
    val min = dim.min + dim.span * ordinal.toDouble / size.toDouble
    val incMin = dim.isMinIncluded || (ordinal == 0L)
    val max = dim.min + dim.span * (1L + ordinal.toDouble) / size.toDouble
    val incMax = dim.isMaxIncluded && (ordinal == (size - 1L))
    RealDimension(min, incMin, max, incMax)
  }
}