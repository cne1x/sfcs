package org.eichelberger.sfc.examples.composition.contrast

import org.eichelberger.sfc._
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.utils.Lexicographics.Lexicographic
import org.joda.time.DateTime

// four-dimensionals...

case class XYZT_C(xPrecision: Int, yPrecision: Int, zPrecision: Int, tPrecision: Int)
  extends ComposedCurve(
    CompactHilbertCurve(xPrecision, yPrecision, zPrecision, tPrecision),
    Seq(
      DefaultDimensions.createLongitude(xPrecision),
      DefaultDimensions.createLatitude(yPrecision),
      DefaultDimensions.createDimension[Double](0.0, 50000.0, zPrecision),
      DefaultDimensions.createDateTime(tPrecision)
    ))

case class XYZT_Z(xPrecision: Int, yPrecision: Int, zPrecision: Int, tPrecision: Int)
  extends ComposedCurve(
    ZCurve(xPrecision, yPrecision, zPrecision, tPrecision),
    Seq(
      DefaultDimensions.createLongitude(xPrecision),
      DefaultDimensions.createLatitude(yPrecision),
      DefaultDimensions.createDimension[Double](0.0, 50000.0, zPrecision),
      DefaultDimensions.createDateTime(tPrecision)
    ))

case class XYZT_R(xPrecision: Int, yPrecision: Int, zPrecision: Int, tPrecision: Int)
  extends ComposedCurve(
    RectilinearCurve(xPrecision, yPrecision, zPrecision, tPrecision),
    Seq(
      DefaultDimensions.createLongitude(xPrecision),
      DefaultDimensions.createLatitude(yPrecision),
      DefaultDimensions.createDimension[Double](0.0, 50000.0, zPrecision),
      DefaultDimensions.createDateTime(tPrecision)
    ))

case class XY_C__Z_R__T_R(xPrecision: Int, yPrecision: Int, zPrecision: Int, tPrecision: Int)
  extends ComposedCurve(
    RectilinearCurve(xPrecision + yPrecision + zPrecision, tPrecision),
    Seq(
      new ComposedCurve(
        RectilinearCurve(xPrecision + yPrecision, zPrecision),
        Seq(
          new ComposedCurve(
            CompactHilbertCurve(xPrecision, yPrecision),
            Seq(
              DefaultDimensions.createLongitude(xPrecision),
              DefaultDimensions.createLatitude(yPrecision)
            )
          ),
          DefaultDimensions.createDimension[Double](0.0, 50000.0, zPrecision)
        )
      ),
      DefaultDimensions.createDateTime(tPrecision)
    )
  )

case class XY_C__ZT_C__R(xPrecision: Int, yPrecision: Int, zPrecision: Int, tPrecision: Int)
  extends ComposedCurve(
    RectilinearCurve(xPrecision + yPrecision, zPrecision + tPrecision),
    Seq(
      new ComposedCurve(
        CompactHilbertCurve(xPrecision, yPrecision),
        Seq(
          DefaultDimensions.createLongitude(xPrecision),
          DefaultDimensions.createLatitude(yPrecision)
        )
      ),
      new ComposedCurve(
        CompactHilbertCurve(zPrecision, tPrecision),
        Seq(
          DefaultDimensions.createDimension[Double](0.0, 50000.0, zPrecision),
          DefaultDimensions.createDateTime(tPrecision)
        )
      )
    )
  )

case class XY_C__ZT_C__C(xPrecision: Int, yPrecision: Int, zPrecision: Int, tPrecision: Int)
  extends ComposedCurve(
    CompactHilbertCurve(xPrecision + yPrecision, zPrecision + tPrecision),
    Seq(
      new ComposedCurve(
        CompactHilbertCurve(xPrecision, yPrecision),
        Seq(
          DefaultDimensions.createLongitude(xPrecision),
          DefaultDimensions.createLatitude(yPrecision)
        )
      ),
      new ComposedCurve(
        CompactHilbertCurve(zPrecision, tPrecision),
        Seq(
          DefaultDimensions.createDimension[Double](0.0, 50000.0, zPrecision),
          DefaultDimensions.createDateTime(tPrecision)
        )
      )
    )
  )

case class XY_C__ZT_C__Z(xPrecision: Int, yPrecision: Int, zPrecision: Int, tPrecision: Int)
  extends ComposedCurve(
    ZCurve(xPrecision + yPrecision, zPrecision + tPrecision),
    Seq(
      new ComposedCurve(
        CompactHilbertCurve(xPrecision, yPrecision),
        Seq(
          DefaultDimensions.createLongitude(xPrecision),
          DefaultDimensions.createLatitude(yPrecision)
        )
      ),
      new ComposedCurve(
        CompactHilbertCurve(zPrecision, tPrecision),
        Seq(
          DefaultDimensions.createDimension[Double](0.0, 50000.0, zPrecision),
          DefaultDimensions.createDateTime(tPrecision)
        )
      )
    )
  )

// two-dimensional geographics...

case class XY_R(xPrecision: Int, yPrecision: Int)
  extends ComposedCurve(
    RectilinearCurve(xPrecision, yPrecision),
    Seq(
      DefaultDimensions.createLongitude(xPrecision),
      DefaultDimensions.createLatitude(yPrecision)
    ))

case class XY_Z(xPrecision: Int, yPrecision: Int)
  extends ComposedCurve(
    ZCurve(xPrecision, yPrecision),
    Seq(
      DefaultDimensions.createLongitude(xPrecision),
      DefaultDimensions.createLatitude(yPrecision)
    ))

case class XY_C(xPrecision: Int, yPrecision: Int)
  extends ComposedCurve(
    CompactHilbertCurve(xPrecision, yPrecision),
    Seq(
      DefaultDimensions.createLongitude(xPrecision),
      DefaultDimensions.createLatitude(yPrecision)
    ))

