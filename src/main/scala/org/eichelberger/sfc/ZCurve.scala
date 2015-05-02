package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.planners.RecursiveQuadTreePlanner
import org.eichelberger.sfc.utils.Lexicographics
import Lexicographics._
import org.eichelberger.sfc.SpaceFillingCurve._

object ZCurve {
  def apply(x: OrdinalNumber*): ZCurve = new ZCurve(OrdinalVector(x: _*))
}

case class ZCurve(val precisions: OrdinalVector) extends SpaceFillingCurve with RecursiveQuadTreePlanner with Lexicographic with LazyLogging {
  import org.eichelberger.sfc.ZCurve._

  val name = "Z"

  // pre-compute bit-to-dimension assignments
  val bitAssignments: Seq[(Int, Int)] = (0 until M).foldLeft((precisions.toSeq, 0, Seq[(Int,Int)]()))((acc, bitPos) => acc match {
    case (precisionsLeft, dimension, assignmentsSoFar) =>
      var nextDim = dimension
      var i = 0
      while (precisionsLeft(nextDim) < 1 && i < n) {
        i = i - 1
        nextDim = (nextDim + 1) % n
      }
      require(precisionsLeft(nextDim) > 0, s"Next dimension $nextDim has a remaining count of ${precisionsLeft(nextDim)}; expected a number greater than zero")

      val nextPrecisionsLeft = precisionsLeft.take(nextDim) ++ Seq(precisionsLeft(nextDim) - 1) ++ precisionsLeft.drop(nextDim + 1)
      val nextDimension = (nextDim + 1) % n
      val nextAssignments = assignmentsSoFar ++ Seq((nextDim, nextPrecisionsLeft(nextDim).toInt))
      (nextPrecisionsLeft, nextDimension, nextAssignments)
  })._3

  def index(point: OrdinalVector): OrdinalNumber = {
    var result = 0L
    var bitPos = 0
    while (bitPos < M) {
      val (dimNum, bitPosInner) = bitAssignments(bitPos)
      result = (result << 1L) | bitAt(point(dimNum), bitPosInner)
      bitPos = bitPos + 1
    }
    result
  }

  def inverseIndex(ordinal: OrdinalNumber): OrdinalVector = {
    var vector = List.fill(n)(0L).toOrdinalVector
    var i = 0
    while (i < M) {
      val (d, _) = bitAssignments(i)
      val newValue = (vector(d) << 1L) | bitAt(ordinal, M - 1 - i)
      vector = vector.set(d, newValue)
      i = i + 1
    }
    vector
  }
}