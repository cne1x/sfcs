package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.utils.Lexicographics
import Lexicographics._
import org.eichelberger.sfc.SpaceFillingCurve._

object ZCurve {
  def apply(x: OrdinalNumber*): ZCurve = new ZCurve(OrdinalVector(x: _*))
}

case class ZCurve(val precisions: OrdinalVector) extends QuadTreeCurve with Lexicographic with LazyLogging {
  import org.eichelberger.sfc.ZCurve._

  // validate precisions
  require(precisions.size > 1, s"Z-curves are meant to operate on two or more dimensions (${precisions.size})")
  for (i <- 1 to precisions.size - 1; prev = precisions(i - 1); curr = precisions(i)) {
    require(curr <= prev, s"Z-curve precisions must be presented in descending order ($curr > $prev)")
    require(curr >= (m - 1), s"Z-curve precisions ($curr) must not be more than 1 bit less than the maximum ($m)")
  }

  def index(point: OrdinalVector): OrdinalNumber = {
    var bitPosInner = 0
    var dimNum = 0
    var bitPosOuter = 0
    var result = 0L
    while (bitPosOuter < M) {
      // accumulate this bit
      val bit = bitAt(point(dimNum), precisions(dimNum).toInt - 1 - bitPosInner)
      result = (result << 1L) | bit
      //result = setBitAt(result, M - 1 - bitPosOuter, bitAt(point(dimNum), precisions(dimNum).toInt - 1 - bitPosInner).toInt)
      
      // increment pointers
      dimNum = dimNum + 1
      if (dimNum >= n) {
        bitPosInner = bitPosInner + 1
        dimNum = 0
      }

      bitPosOuter = bitPosOuter + 1
    }
    
    result
  }

  def inverseIndex(ordinal: OrdinalNumber): OrdinalVector = {
    var vector = List.fill(n)(0L).toOrdinalVector
    
    var i = 0
    while (i < M) {
      val d = i % n
      val newValue = (vector(d) << 1L) | bitAt(ordinal, M - 1 - i)
      vector = vector.set(d, newValue)
      i = i + 1
    }
    vector
  }
}