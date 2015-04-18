package org.eichelberger.sfc.utils

import org.eichelberger.sfc.SpaceFillingCurve._

case class LocalityResult(locality: Double, coverage: Double)

object LocalityEstimator {
  val maxEvaluations = 1L << 16L

  val largestFullIndexSpace =
    Math.floor(0.5 + Math.sqrt(0.5 + 2.0 * maxEvaluations)).toLong

  val maxFullBits = Math.floor(Math.log(largestFullIndexSpace) / Math.log(2.0))

  case class SampleItem(dUser: Double, dIndex: Double)

  type Sample = Seq[SampleItem]
}

case class LocalityEstimator(curve: SpaceFillingCurve) {
  import LocalityEstimator._

  lazy val maxDIndex = curve.size.toDouble
  lazy val maxDUser = Math.sqrt(curve.cardinalities.map(x => x.toDouble * x.toDouble).sum)

  lazy val fullSampleSize = 0.5 * curve.size.toDouble * (curve.size.toDouble - 1.0)

  def sampleItem(a: OrdinalNumber, b: OrdinalNumber): SampleItem = {
    val ptA = curve.inverseIndex(a)
    val ptB = curve.inverseIndex(b)

    val dIndex = Math.abs(a - b)
    val dUser = Math.sqrt(
      ptA.zipWith(ptB).map {
        case (coordA, coordB) => (coordA - coordB) * (coordA - coordB)
      }.sum
    )

    SampleItem(dUser / maxDUser, dIndex / maxDIndex)
  }

  def randomSample: Sample = {
    var sample: Sample = Seq[SampleItem]()

    var i = 0
    while (i < maxEvaluations) {
      val a = Math.floor((curve.size - 1L) * Math.random()).toLong
      val b = Math.floor(a + 1 + (curve.size - a - 1L) * Math.random()).toLong
      sample = sample :+ sampleItem(a, b)
      i = i + 1
    }

    sample
  }

  def fullSample: Sample = {
    var sample: Sample = Seq[SampleItem]()
    var a = 0L
    var b = 0L

    while (a < curve.size - 1L) {
      b = a + 1L
      while (b < curve.size) {
        sample = sample :+ sampleItem(a, b)
        b = b + 1L
      }
      a = a + 1L
    }

    sample
  }

  def covariance(sample: Sample): Double = {
    val x = sample.map(_.dUser)
    val y = sample.map(_.dIndex)

    val xMean = x.sum / x.size.toDouble
    val yMean = y.sum / y.size.toDouble

    val products = x.zip(y).map {
      case (xi, yi) => (xi - xMean) * (yi - yMean)
    }

    products.sum / products.size.toDouble
  }

  def maxAllowableIndexSize: OrdinalNumber =
    Math.floor(0.5 + Math.sqrt(0.5 + 2.0 * maxEvaluations)).toLong

  def locality: LocalityResult = {
    // pull a sample, constrained by a maximum size
    val sample: Sample =
      if (curve.M > maxFullBits) randomSample
      else fullSample

    LocalityResult(
      covariance(sample),
      sample.size.toDouble / fullSampleSize
    )
  }
}
