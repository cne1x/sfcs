package org.eichelberger.sfc.utils

import org.eichelberger.sfc.SpaceFillingCurve._

object Lexicographics {
  case class Alphabet(symbols: Seq[String]) {
    val symbolMap = symbols.zipWithIndex.toMap

    def apply(x: Int): String = symbols(x)

    def size: Int = symbols.size

    val bitsPerSymbol: Int = {
      val bps = Math.round(Math.log(size) / Math.log(2)).toInt
      require((1L << bps) == size, s"Bits-per-symbol of $bps is not equivalent to an alphabet of $size symbols")
      bps
    }
  }
  
  val DefaultAlphabets: Map[Int, Alphabet] = Map(
      1 -> "01",
      2 -> "0123",
      3 -> "01234567",
      4 -> "0123456789abcdef",
      5 -> "0123456789bcdefghjkmnpqrstuvwxyz",
      6 -> "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    )
  
  implicit def string2seq(s: String): Alphabet = 
    Alphabet(s.split("").toSeq)
  
  trait Lexicographic {
    // the total bits precision summed across all dimensions
    def M: Int

    val alphabet: Alphabet = 
      DefaultAlphabets.get(M) match {
        case Some(a) => a
        case None    =>
          // see if you can use an existing alphabet
          val irs = (6 to 1 by -1).zip((6 to 1 by -1).map(M % _))
          val bestOpt = irs.find { case (i, r) => r == 0 }
          // fall back to using a base-2 encoding
          DefaultAlphabets(bestOpt.getOrElse((1,1))._1)
      }
    
    val lexStringLength: Int = {
      require(M % alphabet.bitsPerSymbol == 0, s"The total precision ($M) is not evenly divisible by the alphabet size (${alphabet.size})")
      M / alphabet.bitsPerSymbol
    }
    
    val lexMask: OrdinalNumber = (1L << alphabet.bitsPerSymbol) - 1L
    
    def toBitSource(x: OrdinalNumber): Iterator[OrdinalNumber] =
      (1 to lexStringLength).iterator.map(i => (x >> (M - i * alphabet.bitsPerSymbol)) & lexMask)
    
    def lexEncodeIndex(ordinal: OrdinalNumber): String =
      toBitSource(ordinal).map(value => alphabet(value.toInt)).mkString

    def lexDecodeIndex(hash: String): OrdinalNumber = {
      hash.foldLeft(0L)((ord, symbol) => {
        val bits = alphabet.symbolMap(symbol.toString)
        (ord << alphabet.bitsPerSymbol) | bits
      })
    }
  }
}
