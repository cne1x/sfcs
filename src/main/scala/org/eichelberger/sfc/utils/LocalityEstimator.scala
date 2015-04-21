package org.eichelberger.sfc.utils

import org.eichelberger.sfc.SpaceFillingCurve._

case class LocalityResult(
  locality: Double,
  normalizedLocality: Double,
  localityInverse: Double,
  normalizedLocalityInverse: Double,
  sampleSize: Int,
  coverage: Double)

object LocalityEstimator {
  val maxEvaluations = 1L << 14L

  case class SampleItem(dUser: Double, dIndex: Double)

  type Sample = Seq[SampleItem]
}

case class LocalityEstimator(curve: SpaceFillingCurve) {
  import LocalityEstimator._

  lazy val deltas = (0 until curve.n).toList

  lazy val fullSampleSize = curve.n * curve.cardinalities.product -
    curve.cardinalities.sum

  lazy val maxUserDistance = Math.sqrt(curve.cardinalities.map(c => c * c).sum)

  // items are known to be 1 unit apart in user space
  def sampleItem(a: OrdinalVector, b: OrdinalVector): SampleItem = {
    val idxA: OrdinalNumber = curve.index(a)
    val idxB: OrdinalNumber = curve.index(b)
    SampleItem(1.0, Math.abs(idxA - idxB))
  }

  // items are known to be 1 unit apart in index space
  def sampleItem(a: OrdinalNumber, b: OrdinalNumber): SampleItem = {
    val ptA: OrdinalVector = curve.inverseIndex(a)
    val ptB: OrdinalVector = curve.inverseIndex(b)
    val distUser = Math.sqrt(ptA.zipWith(ptB).map { case (coordA, coordB) =>
      (coordA - coordB) * (coordA - coordB) }.sum)
    SampleItem(distUser, 1.0)
  }

  def randomPoint: OrdinalVector =
    OrdinalVector(deltas.map(i =>
      Math.floor(Math.random() * (curve.cardinalities(i).toDouble - 1.0)).toLong
    ):_*)

  def randomPointAdjacent(a: OrdinalVector): OrdinalVector = {
    while (true) {
      val dim = Math.floor(Math.random() * (curve.n.toDouble - 1.0)).toInt
      val dir = if (Math.random() < 0.5) 1L else -1L
      val newCoord = a(dim) + dir
      if (newCoord >= 0 && newCoord < curve.cardinalities(dim))
        return a.set(dim, newCoord)
    }

    // dummy; this code is never reached
    OrdinalVector()
  }

  def randomSample: Sample = {
    var sample: Sample = Seq[SampleItem]()

    var i = 0
    while (i < maxEvaluations) {
      val a = randomPoint
      val b = randomPointAdjacent(a)
      sample = sample :+ sampleItem(a, b)
      i = i + 1
    }

    sample
  }

  def randomSampleInverse: Sample = {
    val sample = collection.mutable.ListBuffer[SampleItem]()

    var i = 0
    while (i < maxEvaluations) {
      val idx = Math.floor(Math.random() * (curve.n.toDouble - 2.0)).toLong
      sample += sampleItem(idx, idx + 1L)
      i = i + 1
    }

    sample
  }

  def fullSample: Sample = {
    // iterate over all cells in the index space
    val cellIdxItr = combinationsIterator(OrdinalVector(curve.cardinalities:_*))

    (for (
      cellIdx <- cellIdxItr;
      deltaDim <- deltas if cellIdx(deltaDim) + 1L < curve.cardinalities(deltaDim);
      adjCellIdx = cellIdx.set(deltaDim, cellIdx(deltaDim) + 1L)
    ) yield sampleItem(cellIdx, adjCellIdx)).toSeq
  }

  def fullSampleInverse: Sample = {
    val sample = collection.mutable.ListBuffer[SampleItem]()

    var idx: OrdinalNumber = 0
    while ((idx + 1L) < curve.size) {
      sample += sampleItem(idx, idx+1)
      idx = idx + 1L
    }

    sample
  }

  def locality: LocalityResult = {
    // pull a sample, constrained by a maximum size
    val sample: Sample =
      if (fullSampleSize > maxEvaluations) randomSample
      else fullSample

    val absLocality = sample.map(_.dIndex).sum / sample.size.toDouble
    val relLocality = absLocality / curve.size.toDouble

    // pull an inverse sample, constrained by a maximum size
    val sampleInverse: Sample =
      if (fullSampleSize > maxEvaluations) randomSampleInverse
      else fullSampleInverse

    val absLocalityInverse = sampleInverse.map(_.dUser).sum / sample.size.toDouble
    val relLocalityInverse = absLocalityInverse / maxUserDistance

    LocalityResult(
      absLocality,
      relLocality,
      absLocalityInverse,
      relLocalityInverse,
      sample.size,
      sample.size.toDouble / fullSampleSize
    )
  }
}
