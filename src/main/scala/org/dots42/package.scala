package org

import org.dots42.Data.Privacy

import scalaz._, Scalaz._

package object dots42 {

  implicit val pricacyTypeShows = Show.shows[Privacy](x => Privacy.asString(x))

  def generateId: String = {
    val x = scala.util.Random.nextDouble
    val l = (java.lang.Long.MAX_VALUE * x).toLong
    Encoding.dec2base(l)
  }

  object Encoding {

    def encode(table: IndexedSeq[Char])(x: BigInt): String = {
      val base = table.size
      def encode0(x: BigInt, s: String = ""): String = {
        val xp = x / base
        val r = (x % base).toInt
        val c = table(r)
        if (xp > 0) encode0(xp, c + s) else c + s
      }
      encode0(x)
    }

    def decode(table: String)(s: String): BigInt = {
      val base = table.size
      s.toList.map(table.indexOf(_)).map(BigInt(_)).reduceLeft( _ * base + _)
    }

    val encodeTable: IndexedSeq[Char] = ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')

    val hex2dec = decode("0123456789abcdef") _
    val bin2dec = decode("01") _
    val base2dec = decode(encodeTable.mkString) _

    val dec2hex = encode(('0' to '9') ++ ('a' to 'f')) _
    val dec2bin = encode(('0' to '1')) _
    val dec2base = encode(encodeTable) _

  }

}
