package org.eichelberger.sfc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eichelberger.sfc.SpaceFillingCurve.OrdinalVector
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

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

      curve.netPrecisions must equalTo(OrdinalVector(6, 3, 4, 5, 7))
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

      curve.netPrecisions must equalTo(OrdinalVector(6, 7, 5, 7))
    }
  }
}
