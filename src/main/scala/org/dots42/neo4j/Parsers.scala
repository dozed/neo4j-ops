package org.dots42.neo4j

import org.neo4j.cypher.QueryStatistics

import scala.util.Try
import scalaz._, Scalaz._

// Parser[A] converts from Map[String, Any] to A
// - decoders
// - 1-step navigation
object Parsers {

  import Connections._
  import Decoders._

  trait ParseGen[I, A]

  case class Parser[A](run: ResultItem => A) extends ParseGen[ResultItem, A]



  implicit val parserMonad = new Monad[Parser] {
    override def point[A](a: => A): Parser[A] = Parser[A](_ => a)

    override def bind[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] = Parser[B] { res =>
      val a = fa.run(res)
      val b = f(a).run(res)
      b
    }
  }


  // default parser
  implicit val resultItemParser = Parser[ResultItem](identity)


  // primitive parser (+1-step traversal)
  def parse[A: Decoder](key: String): Parser[A] = Parser[A] { m =>
    implicitly[Decoder[A]].run(m(key))
  }

  // -> ParseResult and |
  def parseOrElse[A: Decoder](key: String, default: A): Parser[A] = Parser[A] { m =>
    Try(implicitly[Decoder[A]].run(m(key))).getOrElse(default)
  }


  // parser for common arities
  def parse1[A1:Decoder](k1: String) = parse[A1](k1)

  def parse2[A1:Decoder, A2:Decoder](k1: String, k2: String) = {
    (parse[A1](k1) |@| parse[A2](k2)).tupled
  }

  def parse3[A1:Decoder, A2:Decoder, A3:Decoder](k1: String, k2: String, k3: String) = {
    (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3)).tupled
  }

  def parse4[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder](k1: String, k2: String, k3: String, k4: String) = {
    (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4)).tupled
  }


}
