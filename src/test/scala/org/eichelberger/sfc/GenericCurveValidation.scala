package org.eichelberger.sfc

import org.eichelberger.sfc.SpaceFillingCurve.{OrdinalVector, SpaceFillingCurve, _}

import scala.collection.mutable
import scala.util.{Success, Try}

trait GenericCurveValidation {
  import GenericTesting._

  def curveName: String

  def createCurve(precisions: OrdinalNumber*): SpaceFillingCurve
  
  // utility method for converting a single cell index (not Hilbert ordered,
  // but naively ordered one dimension at a time) to a point
  def idx2pt(sfc: SpaceFillingCurve, idx: Long): OrdinalVector = {
    val rawCards = sfc.precisions.cardinalities.toSeq
    val cumCards: Seq[Long] = rawCards.dropRight(1).foldLeft(Seq[Long](1))((acc, m) =>
      acc ++ Seq(acc.last * m))
    val idxs = cumCards.foldRight((Seq[Long](), idx))((cs, acc) => {
      val (accList, accIdx) = acc
      val placeValue = Math.floor(accIdx / cs).toLong
      (accList ++ Seq(placeValue), accIdx % cs)
    })._1
    CompactHilbertCurve(idxs.reverse:_*).precisions
  }

  def testOrderings(): Boolean = {
    def testOrdering(sfcOpt: Option[SpaceFillingCurve]): Boolean = {
      sfcOpt.foreach(sfc => {
        val id = sfc.precisions.cardinalities.toSeq.mkString(" x ")
        println(s"Validating $curveName $id space...")
        
        var everSeen = mutable.BitSet()

        var i = 0
        while (i < sfc.size.toInt) {
          val point = idx2pt(sfc, i)
          val h = sfc.index(point)
          val point2 = sfc.inverseIndex(h)

          // validate the index range
          if (h >= sfc.size) throw new Exception(s"Index overflow:  $h >= ${sfc.size}")
          if (h < 0) throw new Exception(s"Index underflow:  $h < 0")

          // must have an inverse that maps to this cell's point
          if (point2 != point) throw new Exception(s"Invalid round-trip:  input $point -> #$h -> return $point2")

          // accumulate this index
          if (everSeen(h.toInt)) throw new Exception(s"Index collision:  input $point -> $h")
          everSeen = everSeen + h.toInt

          i = i + 1
        }

        i = 0
        while (i < sfc.size.toInt) {
          if (!everSeen(i)) throw new Exception(s"Unseen index value:  $i")
          i = i + 1
        }
      })

      // if you get here, the test was successful
      true
    }

    def considerTestingOrdering(ms: Int*): Unit = {
      testOrdering(Try {
        val pp: Seq[OrdinalNumber] = ms.map(_.toOrdinalNumber)
        createCurve(pp:_*)
      }.toOption)
    }
    
    // 2-dimensions:  allocate all valid index values exactly once
    def test2d(): Boolean = {
      for (m0 <- 1 to 5; m1 <- 1 to 5)
        considerTestingOrdering(m0, m1)
      true
    }

    // 3-dimensions:  allocate all valid index values exactly once
    def test3d(): Boolean = {
      for (m0 <- 1 to 5; m1 <- 1 to 5; m2 <- 1 to 5)
        considerTestingOrdering(m0, m1, m2)
      true
    }

    // 4-dimensions:  allocate all valid index values exactly once
    def test4d(): Boolean = {
      for (m0 <- 1 to 4; m1 <- 1 to 4; m2 <- 1 to 4; m3 <- 1 to 4)
        considerTestingOrdering(m0, m1, m2, m3)
      true
    }

    // actually conduct the tests; will throw exceptions if not everything works
    test2d() && test3d() && test4d()
  }
  
  def timeTestOrderings(): Boolean = {
    val (result, msDuration) = time(testOrderings)
    println(s"\n[CURVE VALIDATION TIMING] $curveName:  ${(msDuration.toDouble / 1000.0).formatted("%1.3f")} sec\n")
    result
  }
}
