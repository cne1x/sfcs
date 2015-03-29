package org.eichelberger.sfc.examples.composition.contrast

import org.eichelberger.sfc._
import org.eichelberger.sfc.SpaceFillingCurve._
import org.eichelberger.sfc.utils.Lexicographics.Lexicographic
import org.joda.time.DateTime

// four-dimensionals...

case class FactoryXYZT(precision: Int, plys: Int) {
  def buildVerticalCurve(combination: OrdinalVector, precision: Int): ComposedCurve = {
    val (leftPrec, rightPrec) = combination.toSeq.last match {
      case 1 =>
        val b = precision >> 1
        val a = precision - b
        (a, b)
      case _ =>
        val b = precision / (combination.size + 1)
        val a = precision - b
        (a, b)
    }

    val rightChild: Composable  = combination.size match {
      case 3 => DefaultDimensions.createDateTime(rightPrec)
      case 2 => DefaultDimensions.createDimension[Double]("z", 0.0, 50000.0, rightPrec)
      case 1 => DefaultDimensions.createLatitude(rightPrec)
      case _ => throw new Exception("Invalid right child specification")
    }
    val leftChild: Composable = combination.size match {
      case 3 | 2 => buildVerticalCurve(OrdinalVector(combination.toSeq.dropRight(1):_*), leftPrec)
      case 1 => DefaultDimensions.createLongitude(leftPrec)
      case _ => throw new Exception("Invalid left child specification")
    }

    val rawCurve = combination.toSeq.last match {
      case 0 => RectilinearCurve(leftPrec, rightPrec)
      case 1 => ZCurve(leftPrec, rightPrec)
      case 2 => CompactHilbertCurve(leftPrec, rightPrec)
    }

    new ComposedCurve(rawCurve, Seq(leftChild, rightChild))
  }

  def buildMixedCurve(combination: OrdinalVector, precision: Int, side: Int = 0): ComposedCurve = {
    val (leftPrec, rightPrec) = combination.toSeq.last match {
      case 1 =>
        val b = precision >> 1
        val a = precision - b
        (a, b)
      case _ =>
        val b = precision / (combination.size + 1)
        val a = precision - b
        (a, b)
    }

    val leftChild: Composable = side match {
      case 0 => buildMixedCurve(OrdinalVector(combination(0)), leftPrec, 1)
      case 1 => DefaultDimensions.createLongitude(leftPrec)
      case 2 => DefaultDimensions.createDimension[Double]("z", 0.0, 50000.0, leftPrec)
    }
    val rightChild: Composable = side match {
      case 0 => buildMixedCurve(OrdinalVector(combination(1)), rightPrec, 2)
      case 1 => DefaultDimensions.createLatitude(rightPrec)
      case 2 => DefaultDimensions.createDateTime(rightPrec)
    }

    val rawCurve = combination.toSeq.last match {
      case 0 => RectilinearCurve(leftPrec, rightPrec)
      case 1 => ZCurve(leftPrec, rightPrec)
      case 2 => CompactHilbertCurve(leftPrec, rightPrec)
    }

    new ComposedCurve(rawCurve, Seq(leftChild, rightChild))
  }

  def buildHorizontalCurve(combination: OrdinalVector, precision: Int): ComposedCurve = {
    val tPrecision = precision / 4
    val zPrecision = (precision - tPrecision) / 3
    val yPrecision = (precision - tPrecision - zPrecision) >> 1
    val xPrecision = precision - tPrecision - zPrecision - yPrecision

    val rawCurve = combination.toSeq.last match {
      case 0 => RectilinearCurve(xPrecision, yPrecision, zPrecision, tPrecision)
      case 1 => ZCurve(xPrecision, yPrecision, zPrecision, tPrecision)
      case 2 => CompactHilbertCurve(xPrecision, yPrecision, zPrecision, tPrecision)
    }

    new ComposedCurve(
      rawCurve,
      Seq(
        DefaultDimensions.createLongitude(xPrecision),
        DefaultDimensions.createLatitude(yPrecision),
        DefaultDimensions.createDimension[Double]("z", 0.0, 50000.0, zPrecision),
        DefaultDimensions.createDateTime(tPrecision)
      )
    )
  }

  def getCurves: Seq[ComposedCurve] = {
    plys match {
      case 3 =>  // vertical
        combinationsIterator(OrdinalVector(3, 3, 3)).toList.map(combination => buildVerticalCurve(combination, precision))
      case 2 =>  // mixed
        combinationsIterator(OrdinalVector(3, 3, 3)).toList.map(combination => buildMixedCurve(combination, precision))
      case 1 =>  // horizontal
        combinationsIterator(OrdinalVector(3)).toList.map(combination => buildHorizontalCurve(combination, precision))
    }
  }
}

// three-dimensionals...

case class FactoryXYT(precision: Int, plys: Int) {
  def buildVerticalCurve(combination: OrdinalVector, precision: Int): ComposedCurve = {
    val (leftPrec, rightPrec) = combination.toSeq.last match {
      case 1 =>
        val b = precision >> 1
        val a = precision - b
        (a, b)
      case _ =>
        val b = precision / (combination.size + 1)
        val a = precision - b
        (a, b)
    }

    val rightChild: Composable  = combination.size match {
      case 2 => DefaultDimensions.createDateTime(rightPrec)
      case 1 => DefaultDimensions.createLatitude(rightPrec)
      case _ => throw new Exception("Invalid right child specification")
    }
    val leftChild: Composable = combination.size match {
      case 2 => buildVerticalCurve(OrdinalVector(combination.toSeq.dropRight(1):_*), leftPrec)
      case 1 => DefaultDimensions.createLongitude(leftPrec)
      case _ => throw new Exception("Invalid left child specification")
    }

    val rawCurve = combination.toSeq.last match {
      case 0 => RectilinearCurve(leftPrec, rightPrec)
      case 1 => ZCurve(leftPrec, rightPrec)
      case 2 => CompactHilbertCurve(leftPrec, rightPrec)
    }

    new ComposedCurve(rawCurve, Seq(leftChild, rightChild))
  }

  def buildHorizontalCurve(combination: OrdinalVector, precision: Int): ComposedCurve = {
    val tPrecision = precision / 3
    val yPrecision = (precision - tPrecision) >> 1
    val xPrecision = precision - tPrecision - yPrecision

    val rawCurve = combination.toSeq.last match {
      case 0 => RectilinearCurve(xPrecision, yPrecision, tPrecision)
      case 1 => ZCurve(xPrecision, yPrecision, tPrecision)
      case 2 => CompactHilbertCurve(xPrecision, yPrecision, tPrecision)
    }

    new ComposedCurve(
      rawCurve,
      Seq(
        DefaultDimensions.createLongitude(xPrecision),
        DefaultDimensions.createLatitude(yPrecision),
        DefaultDimensions.createDateTime(tPrecision)
      )
    )
  }

  def getCurves: Seq[ComposedCurve] = {
    plys match {
      case 2 =>  // mixed
        combinationsIterator(OrdinalVector(3, 3)).toList.map(combination => buildVerticalCurve(combination, precision))
      case 1 =>  // horizontal
        combinationsIterator(OrdinalVector(3)).toList.map(combination => buildHorizontalCurve(combination, precision))
    }
  }
}

// two-dimensionals...

case class FactoryXY(precision: Int) {
  def buildCurve(combination: OrdinalVector, precision: Int): ComposedCurve = {
    val (leftPrec, rightPrec) = combination.toSeq.last match {
      case 1 =>
        val b = precision >> 1
        val a = precision - b
        (a, b)
      case _ =>
        val b = precision / (combination.size + 1)
        val a = precision - b
        (a, b)
    }

    val rightChild: Composable = DefaultDimensions.createLatitude(rightPrec)
    val leftChild: Composable = DefaultDimensions.createLongitude(leftPrec)

    val rawCurve = combination.toSeq.last match {
      case 0 => RectilinearCurve(leftPrec, rightPrec)
      case 1 => ZCurve(leftPrec, rightPrec)
      case 2 => CompactHilbertCurve(leftPrec, rightPrec)
    }

    new ComposedCurve(rawCurve, Seq(leftChild, rightChild))
  }

  def getCurves: Seq[ComposedCurve] =
    combinationsIterator(OrdinalVector(3)).toList.map(combination => buildCurve(combination, precision))
}
