package org.eichelberger.sfc.study.planner

import org.eichelberger.sfc.SpaceFillingCurve
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.planners.{SquareQuadTreePlanner, OffSquareQuadTreePlanner}
import org.eichelberger.sfc.utils.Timing
import org.eichelberger.sfc._

object PlannerStudy extends App {
  trait IndexCounter extends SpaceFillingCurve {
    var numIndexed: Long = 0L

    abstract override def index(point: OrdinalVector): OrdinalNumber = {
      numIndexed += 1
      super.index(point)
    }
  }

  class NewR(precisions: OrdinalVector) extends RowMajorCurve(precisions) with IndexCounter

  class NewZ(precisions: OrdinalVector) extends ZCurve(precisions) with IndexCounter

  class NewH(precisions: OrdinalVector) extends CompactHilbertCurve(precisions) with IndexCounter

  val precisions = OrdinalVector(20, 30)

  val pointQuery = Query(Seq(
    OrdinalRanges(OrdinalPair(3, 3)),
    OrdinalRanges(OrdinalPair(19710507, 19710507))
  ))
  val smallQuery = Query(Seq(
    OrdinalRanges(OrdinalPair(1970, 2001)),
    OrdinalRanges(OrdinalPair(423, 828))
  ))
  val mediumQuery = Query(Seq(
    OrdinalRanges(OrdinalPair(19704, 20018)),
    OrdinalRanges(OrdinalPair(4230, 8281))
  ))
  val bigQuery = Query(Seq(
    OrdinalRanges(OrdinalPair(2, 28), OrdinalPair(101, 159)),
    OrdinalRanges(OrdinalPair(19710507, 20010423))
  ))
  val query = mediumQuery

  val r = new NewR(precisions)
  val z = new NewZ(precisions)
  val h = new NewH(precisions)

  //@TODO restore higher counts!
  val numWarmup = 1
  val numEval = 5

  (1 to numWarmup) foreach { i =>
    val (rR, msR) = Timing.time(() => r.getRangesCoveringQuery(query))
    println(s"warmup R $i in $msR ms...")
    val (rZ, msZ) = Timing.time(() => z.getRangesCoveringQuery(query))
    println(s"warmup Z $i in $msZ ms...")
    val (rH, msH) = Timing.time(() => h.getRangesCoveringQuery(query))
    println(s"warmup H $i in $msH ms...")

    if (i == 1) {
      val rListR = rR.toList
      val rSize = rListR.size
      val rCells = rListR.map(_.size).sum
      println(s"\n  Number of R ranges:  $rSize\n               cells:  $rCells")

      val rListZ = rZ.toList
      val zSize = rListZ.size
      val zCells = rListZ.map(_.size).sum
      println(s"\n  Number of Z ranges:  $zSize\n               cells:  $zCells")

      val rListH = rH.toList
      val hSize = rListH.size
      val hCells = rListH.toList.map(_.size).sum
      println(s"\n  Number of H ranges:  $hSize\n                cells:  $hCells")

      println(s"\n  Number of R evaluations:  ${r.numIndexed}")
      println(s"  Number of Z evaluations:  ${z.numIndexed}")
      println(s"  Number of H evaluations:  ${h.numIndexed}")
    }
  }

  val (msSumR, msSumZ, msSumH) = (1 to numEval).foldLeft((0L, 0L, 0L))((acc, i) => acc match {
    case (soFarR, soFarZ, soFarH) =>
      val (_, msR) = Timing.time(() => r.getRangesCoveringQuery(query))
      println(s"evaluated R $i in $msR ms...")
      val (_, msZ) = Timing.time(() => z.getRangesCoveringQuery(query))
      println(s"evaluated Z $i in $msZ ms...")
      val (rH1, msH1) = Timing.time(() => h.getRangesCoveringQuery(query))
      println(s"evaluated H1 $i in $msH1 ms...")

      (soFarR + msR, soFarZ + msZ, soFarH + msH1)
  })
  val msR = msSumR / numEval.toDouble / 1000.0
  val msZ = msSumZ / numEval.toDouble / 1000.0
  val msH = msSumH / numEval.toDouble / 1000.0

  println(s"[PLANNER TIMING STUDY]")
  println(s"\nNumber of warm-up trials:  $numWarmup")
  println(s"Number of evaluation trials:  $numEval")
  println(s"\nR mean planning time:  ${msR.formatted("%1.4f")} seconds")
  println(s"Z mean planning time:  ${msZ.formatted("%1.4f")} seconds")
  println(s"H1 mean planning time:  ${msH.formatted("%1.4f")} seconds")
}