package org.dots42.neo4j

import scala.util.Try
import scalaz._, Scalaz._
import collection.JavaConversions._


// Parser[A] converts from Map[String, Any] to A
// - decoders
// - 1-step navigation
object Parsers {

  import Connections._
  import Decoders._

  trait ParseGen[I, A]

  case class Parser[A](run: ResultItem => A) extends ParseGen[ResultItem, A]

  object Parser extends ParserInstances with TupleParsers {

    def instance[A](run: ResultItem => A): Parser[A] = Parser(run)

    def parse[A: Decoder](key: String): Parser[A] = Parser[A] { m =>
      implicitly[Decoder[A]].run(m(key))
    }

  }

  implicit val parserMonad = new Monad[Parser] {
    override def point[A](a: => A): Parser[A] = Parser[A](_ => a)

    override def bind[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] = Parser[B] { res =>
      val a = fa.run(res)
      val b = f(a).run(res)
      b
    }
  }


  // default parser

  trait ParserInstances {

    implicit val resultItemParser = Parser.instance[ResultItem](identity)

    implicit val unitParser = ().point[Parser]

  }

  implicit def parserDecoder[A](implicit parser: Parser[A]): Decoder[A] = Decoder.instance[A] {
    any =>
      val map = any.asInstanceOf[java.util.Map[String, Any]].toMap
      parser.run(map)
  }


  // -> ParseResult and |
  def parseOrElse[A: Decoder](key: String, default: A): Parser[A] = Parser[A] { m =>
    Try(implicitly[Decoder[A]].run(m(key))).getOrElse(default)
  }

  implicit class ParserExt[A](p: Parser[A]) {
    def |(q: => Parser[A]): Parser[A] = Parser[A] { m =>
      Try(p.run(m)).filter(_ != null).getOrElse(q.run(m))
    }
  }

  trait TupleParsers {

    import Parser.parse

    def tuple1[A1:Decoder](k1: String) = parse[A1](k1)

    def tuple2[A1:Decoder, A2:Decoder](k1: String, k2: String) = {
      (parse[A1](k1) |@| parse[A2](k2)).tupled
    }

    def tuple3[A1:Decoder, A2:Decoder, A3:Decoder](k1: String, k2: String, k3: String) = {
      (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3)).tupled
    }

    def tuple4[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder](k1: String, k2: String, k3: String, k4: String) = {
      (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4)).tupled
    }

    def tuple6[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder, A5:Decoder, A6:Decoder](k1: String, k2: String, k3: String, k4: String, k5: String, k6: String) = {
      (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4) |@| parse[A5](k5) |@| parse[A6](k6)).tupled
    }

    def tuple7[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder, A5:Decoder, A6:Decoder, A7:Decoder](k1: String, k2: String, k3: String, k4: String, k5: String, k6: String, k7: String) = {
      (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4) |@| parse[A5](k5) |@| parse[A6](k6) |@| parse[A7](k7)).tupled
    }

    def tuple8[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder, A5:Decoder, A6:Decoder, A7:Decoder, A8:Decoder](k1: String, k2: String, k3: String, k4: String, k5: String, k6: String, k7: String, k8: String) = {
      (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4) |@| parse[A5](k5) |@| parse[A6](k6) |@| parse[A7](k7) |@| parse[A8](k8)).tupled
    }

    def tuple9[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder, A5:Decoder, A6:Decoder, A7:Decoder, A8:Decoder, A9:Decoder](k1: String, k2: String, k3: String, k4: String, k5: String, k6: String, k7: String, k8: String, k9: String) = {
      (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4) |@| parse[A5](k5) |@| parse[A6](k6) |@| parse[A7](k7) |@| parse[A8](k8) |@| parse[A9](k9)).tupled
    }

    def tuple10[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder, A5:Decoder, A6:Decoder, A7:Decoder, A8:Decoder, A9:Decoder, A10:Decoder](k1: String, k2: String, k3: String, k4: String, k5: String, k6: String, k7: String, k8: String, k9: String, k10: String) = {
      (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4) |@| parse[A5](k5) |@| parse[A6](k6) |@| parse[A7](k7) |@| parse[A8](k8) |@| parse[A9](k9) |@| parse[A10](k10)).tupled
    }

  }

}
