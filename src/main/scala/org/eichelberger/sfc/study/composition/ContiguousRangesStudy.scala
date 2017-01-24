package org.eichelberger.sfc.study.composition

import org.eichelberger.sfc.SpaceFillingCurve.{OrdinalVector, OrdinalPair}
import org.eichelberger.sfc.utils.CompositionParser

object ContiguousRangesStudy extends App {
  def combinationsPerDimension(cardinality: Long) =
    (cardinality * (cardinality + 1L)) >> 1L

  def combinationsPerCube(cardinalities: Seq[Long]) =
    cardinalities.foldLeft(1L)((acc, cardinality) => acc * combinationsPerDimension(cardinality))

  def queryIterator(precisions: OrdinalVector) = new Iterator[Seq[OrdinalPair]] {
    val n = precisions.size
    val cardinalities: Seq[Long] = precisions.toSeq.map(p => 1L << p)
    val MaxCardinality = combinationsPerCube(cardinalities)

    var state: Long = 0

    def getDimRange(n: Long, c: Long): OrdinalPair = {
      var inc = c
      var sum = inc
      while (sum <= n) {
        inc -= 1
        sum += inc
      }
      val min = c - inc
      val max = c - sum + n
      OrdinalPair(min, max)
    }

    def hasNext: Boolean = state < MaxCardinality

    def next(): Seq[OrdinalPair] = {
      var stateCopy = state
      val result = for (i <- 0 until n) yield {
        val dimIndex = stateCopy % cardinalities(i)
        stateCopy /= cardinalities(i).toLong
        getDimRange(dimIndex, cardinalities(i))
      }
      state += 1L
      result
    }
  }

  val Precision = 3

  val curves = Seq(
    s"R($Precision, $Precision, $Precision)",
    s"Z($Precision, $Precision, $Precision)",
    s"H($Precision, $Precision, $Precision)",
    s"R($Precision, H($Precision, $Precision))",
    s"R(H($Precision, $Precision), $Precision)"
  ).map(cString => CompositionParser.buildWholeNumberCurve(cString))
}
