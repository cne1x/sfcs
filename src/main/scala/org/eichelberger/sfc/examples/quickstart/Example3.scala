package org.eichelberger.sfc.examples.quickstart

object Example3 extends App {
  import org.eichelberger.sfc._
  import org.eichelberger.sfc.Dimensions._
  import org.eichelberger.sfc.SpaceFillingCurve._
  import org.joda.time.{DateTimeZone, DateTime}

  // create the dimensions that can manage user-space
  val x = DefaultDimensions.createLongitude(18)  // ~153 meters per cell (@ equator)
  val y = DefaultDimensions.createLatitude(17)   // ~153 meters per cell
  val t = DefaultDimensions.createNearDateTime(
      new DateTime(1970, 1, 1, 0, 0, 0, DateTimeZone.forID("UTC")),
      new DateTime(2010, 12, 31, 23, 59, 59, DateTimeZone.forID("UTC")),
      20
    )

  // compose the curve with dimensions
  val curve = new ComposedCurve(
    RowMajorCurve(20, 35),
    Seq(
      t,
      new ComposedCurve(
        CompactHilbertCurve(18, 17),
        Seq(x, y)
      )
    )
  )

  // hashing points in user space
  val point = Seq(
    new DateTime(1998, 4, 7, 21, 15, 11, DateTimeZone.forID("UTC")),  // t
    -78.49,                                                           // x
    38.04                                                             // y
  )
  val hash = curve.pointToHash(point)
  println(s"$point -> $hash")

  // fetching user-space cells from hash value
  val cell = curve.hashToCell(hash)
  println(s"$cell <- $hash")

  // identify the top-level index-ranges that cover a query
  val query = Cell(Seq(
    DefaultDimensions.createNearDateTime(
      new DateTime(1998, 6, 15, 0, 0, 0, DateTimeZone.forID("UTC")),
      new DateTime(1998, 7, 15, 23, 59, 59, DateTimeZone.forID("UTC")),
      0
    ),
    DefaultDimensions.createDimension("x", -80.0, -79.0, 0),
    DefaultDimensions.createDimension("y", 38.0, 39.0, 0)
  ))
  val ranges = curve.getRangesCoveringCell(query).toList
  println(s"Number of ranges:  ${ranges.size}")
  val totalCells = ranges.map(_.size).sum
  println(s"Total cells in ranges:  $totalCells")
}
