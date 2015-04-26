package org.eichelberger.sfc.study

import java.io.{FileWriter, BufferedWriter, PrintWriter}

case class ColumnSpec(name: String, isQuoted: Boolean)

case class OutputMetadata(columnSpecs: Seq[ColumnSpec])

trait OutputDestination {
  def print(s: String): Unit
  def println(s: String): Unit
  def println(record: Seq[Any]): Unit
  def close(): Unit
}

trait FileOutputDestination extends OutputDestination {
  def fileName: String

  // create a PrintWriter that will automatically flush
  lazy val pw = new PrintWriter(new BufferedWriter(new FileWriter(fileName)), true)

  def print(s: String): Unit = pw.print(s)

  def println(s: String): Unit = pw.println(s)

  def close(): Unit = {
    pw.flush()
    pw.close()
  }
}

trait ScreenOutputDestination extends OutputDestination {
  lazy val pw = System.out

  def print(s: String): Unit = pw.print(s)

  def println(s: String): Unit = pw.println(s)

  def close(): Unit = {
    pw.flush()
    pw.close()
  }
}

trait OutputFormat {
  this: OutputDestination =>
  
  def prepareToClose(): Unit = {}
}

class DelimitedTextFile(fileName: String, metadata: OutputMetadata, writeHeader: Boolean, fieldSeparator: String, encloser: String) extends OutputFormat {
  this: OutputDestination =>

  // number of fields
  val n = metadata.columnSpecs.size

  if (writeHeader) {
    // write header line
    println(metadata.columnSpecs.map(spec => encloser + spec.name + encloser).mkString(fieldSeparator))
  }

  def println(record: Seq[Any]): Unit = {
    require(record.size <= n, s"Too many fields to write out")
    val values: Seq[String] = record.padTo(n, null).zip(metadata.columnSpecs).map {
      case (field, spec) =>
        val fieldStr =
          if (field == null) ""
          else field.toString
        val outStr =
          if (spec.isQuoted) encloser + fieldStr + encloser
          else fieldStr
        outStr
    }
    println(values.mkString(fieldSeparator))
  }
}

abstract class CSV(fileName: String, metadata: OutputMetadata, writeHeader: Boolean)
  extends DelimitedTextFile(fileName, metadata, writeHeader, ",", "\"") {
  this: OutputDestination =>

}

abstract class TSV(fileName: String, metadata: OutputMetadata, writeHeader: Boolean)
  extends DelimitedTextFile(fileName, metadata, writeHeader, "\t", "") {
  this: OutputDestination =>

}

abstract class JSON(fileName: String, metadata: OutputMetadata) {
  this: OutputDestination =>

  print("var data = [")

  val quote = "\""

  var needsComma = false

  def println(values: Seq[Any]): Unit = {
    if (needsComma) print(",")
    println("\n\t{")
    values.zip(metadata.columnSpecs).zipWithIndex.foreach {
      case ((field, spec), i) =>
        val fieldStr =
          if (field == null) ""
          else field.toString
        val outStr =
          if (spec.isQuoted) quote + fieldStr + quote
          else fieldStr
        if (i > 0) println(",")
        print("\t\t" + quote + spec.name + quote + ": " + outStr)
    }
    print("\n\t}")

    needsComma = true
  }

  def prepareToClose(): Unit = {
    println("\n];")
    close()
  }
}

class MultipleOutput(children: Seq[OutputDestination]) extends OutputDestination {
  def print(s: String): Unit = children.foreach(_.print(s))
  def println(s: String): Unit = children.foreach(_.println(s))
  def println(values: Seq[Any]): Unit = children.foreach(_.println(values))
  override def close(): Unit = children.foreach(_.close())
}

class MirroredCSV(val fn: String, metadata: OutputMetadata, writeHeader: Boolean)
  extends MultipleOutput(Seq(
    new CSV(fn, metadata, writeHeader) with FileOutputDestination { def fileName = fn },
    new CSV(fn, metadata, writeHeader) with ScreenOutputDestination
  ))

class MirroredTSV(val fn: String, metadata: OutputMetadata, writeHeader: Boolean)
  extends MultipleOutput(Seq(
    new TSV(fn, metadata, writeHeader) with FileOutputDestination { def fileName = fn },
    new TSV(fn, metadata, writeHeader) with ScreenOutputDestination
  ))

class MirroredJSON(val fn: String, metadata: OutputMetadata, writeHeader: Boolean)
  extends MultipleOutput(Seq(
    new JSON(fn, metadata) with FileOutputDestination { def fileName = fn },
    new JSON(fn, metadata) with ScreenOutputDestination
  ))
