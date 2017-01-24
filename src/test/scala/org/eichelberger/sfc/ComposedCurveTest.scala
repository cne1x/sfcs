package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.SpaceFillingCurve._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.eichelberger.sfc.examples.Geohash

@RunWith(classOf[JUnitRunner])
class ComposedCurveTest extends Specification with LazyLogging {
  sequential

  "net precisions" should {
    "work correctly on a full curve" >> {
      val curve = new ComposedCurve(
        RowMajorCurve(6, 12, 7),
        Seq(
          DefaultDimensions.createLongitude(6),
          new ComposedCurve(
            ZCurve(7, 5),
            Seq(
              new ComposedCurve(
                CompactHilbertCurve(3, 4),
                Seq(
                  DefaultDimensions.createDateTime(3),
                  DefaultDimensions.createDimension("altitude", 0.0, 50000.0, 4)
                )
              ),
              DefaultDimensions.createDimension("IBU", 0.0, 100.0, 5)
            )
          ),
          DefaultDimensions.createLatitude(7)
        )
      )

      curve.precisions must equalTo(OrdinalVector(6, 3, 4, 5, 7))
    }

    /*
     */
    "test GeoMesa plan" >> {
      //41 28, 45 28, 45 33, 41 33, 41 28
      for (bits <- 11 to 41) {
        val curve = new Geohash(bits)
        val cellQuery = Cell(Seq(
          DefaultDimensions.createDimension("x", 41.0, 45.0, 0),
          DefaultDimensions.createDimension("y", 28.0, 33.0, 0)
        ))
        val ranges = curve.getRangesCoveringCell(cellQuery)
        println(s"[GEOMESA-905]  Number of index-ranges at $bits bits:  ${ranges.length}")
      }
      1 must equalTo(1)
    }

    "GeoMesa test" >> {
      val bits = 60

      val pXY = bits * 2 / 3
      val pY = pXY >> 1
      val pX = pXY - pY
      val pT = bits - pXY

      val dimX = DefaultDimensions.createLongitude(pX)
      val dimY = DefaultDimensions.createLatitude(pY)
      val dimT = DefaultDimensions.createIdentityDimension("t1", pT)

      val curve = new ComposedCurve(
        RowMajorCurve(pX, pY, pT),
        Seq(dimX, dimY, dimT)
      )

      val (iX0: Long, iX1: Long) = (dimX.index(41.0), dimX.index(45.0))
      val (iY0: Long, iY1: Long) = (dimY.index(28.0), dimY.index(33.0))
      val (iT0: Long, iT1: Long) = (dimT.min, dimT.max)

      val query = Query(Seq(
        OrdinalRanges(OrdinalPair(iX0, iX1)),
        OrdinalRanges(OrdinalPair(iY0, iY1)),
        OrdinalRanges(OrdinalPair(iT0, iT1))
      ))

      val qdimX = DefaultDimensions.createDimension[Double]("x", iX0, iX1, 0)
      val qdimY = DefaultDimensions.createDimension[Double]("y", iY0, iY1, 0)
      val qdimT = DefaultDimensions.createDimension[Long]("t", iT0, iT1, 0)

      val cellQuery: Cell = Cell(Seq(qdimX, qdimY, qdimT))

      val ranges = curve.getRangesCoveringCell(cellQuery).toList

      println(s"[GEOMESA-905] bits $bits ($pX X, $pY Y, $pT T), query($iX0-$iX1, $iY0-$iY1, $iT0-$iT1), ranges ${ranges.length}")
      println(s"  Range 0:  ${ranges.head}")
      for (range <- ranges; idx <- range.min to Math.min(range.min + 8, range.max)) {
        val point = curve.inverseIndex(idx)
        println(s"  index $idx, point $point")
      }

      1 must equalTo(1)
    }

    "work correctly on a partial curve" >> {
      val curve = new ComposedCurve(
        RowMajorCurve(6, 12, 7),
        Seq(
          DefaultDimensions.createLongitude(6),
          ZCurve(7, 5),
          DefaultDimensions.createLatitude(7)
        )
      )

      curve.precisions must equalTo(OrdinalVector(6, 7, 5, 7))
    }
  }
}
