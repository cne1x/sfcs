package org.eichelberger.sfc.examples.composition.contrast

import java.io.{FileWriter, BufferedWriter, PrintWriter}

case class ColumnSpec(name: String, isQuoted: Boolean)

case class OutputMetadata(columnSpecs: Seq[ColumnSpec])

trait OutputFormat {
  def println(record: Seq[Any]): Unit
  def close(): Unit
}

abstract class FileOutputFormat(fileName: String) extends OutputFormat {
  // create a PrintWriter that will automatically flush
  val pw = new PrintWriter(new BufferedWriter(new FileWriter(fileName)), true)

  def close(): Unit = {
    pw.flush()
    pw.close()
  }
}

class DelimitedTextFile(fileName: String, metadata: OutputMetadata, writeHeader: Boolean, fieldSeparator: String, encloser: String) extends FileOutputFormat(fileName) {
  // number of fields
  val n = metadata.columnSpecs.size

  if (writeHeader) {
    // write header line
    pw.println(metadata.columnSpecs.map(spec => encloser + spec.name + encloser).mkString(fieldSeparator))
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
    pw.println(values.mkString(fieldSeparator))
  }
}

case class CSV(fileName: String, metadata: OutputMetadata, writeHeader: Boolean)
  extends DelimitedTextFile(fileName, metadata, writeHeader, ",", "\"")

case class TSV(fileName: String, metadata: OutputMetadata, writeHeader: Boolean)
  extends DelimitedTextFile(fileName, metadata, writeHeader, "\t", "")


case class JSON(fileName: String, metadata: OutputMetadata) extends FileOutputFormat(fileName) {
  pw.print("var data = [")

  val quote = "\""

  var needsComma = false

  def println(values: Seq[Any]): Unit = {
    if (needsComma) pw.print(",")
    pw.println("\n\t{")
    values.zip(metadata.columnSpecs).zipWithIndex.foreach {
      case ((field, spec), i) =>
        val fieldStr =
          if (field == null) ""
          else field.toString
        val outStr =
          if (spec.isQuoted) quote + fieldStr + quote
          else fieldStr
        if (i > 0) pw.println(",")
        pw.print("\t\t" + quote + spec.name + quote + ": " + outStr)
    }
    pw.print("\n\t}")

    needsComma = true
  }

  override def close(): Unit = {
    pw.println("\n];")
    super.close()
  }
}


case class MultipleOutput(children: Seq[OutputFormat]) extends OutputFormat {
  def println(values: Seq[Any]): Unit = children.foreach(_.println(values))
  def close(): Unit = children.foreach(_.close())
}