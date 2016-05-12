package org.dots42.neo4j

import scala.util.Try
import scalaz._, Scalaz._

// Parser[A] converts from Map[String, Any] to A
trait ParserTypes {

  trait ParseGen[I, A] {
    def run: I => A
  }

  trait Parser[A] extends ParseGen[ResultItem, A] {
    def run: ResultItem => A
  }

  object Parser {

    def apply[A:Parser]: Parser[A] = implicitly[Parser[A]]

    def instance[A](f: ResultItem => A): Parser[A] = new Parser[A] {
      override def run: (ResultItem) => A = rs => f(rs)
    }

  }


}

trait Parsers {

  implicit val parserMonad = new Monad[Parser] {
    override def point[A](a: => A): Parser[A] = Parser.instance[A](_ => a)

    override def bind[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] = Parser.instance[B] { res =>
      val a = fa.run(res)
      val b = f(a).run(res)
      b
    }
  }


  // default parser
  implicit val resultItemParser = Parser.instance[ResultItem](identity)

  implicit val unitParser = ().point[Parser]

  implicit def decoderParser[A](implicit parser: Parser[A]): Decoder[A] = Decoder.instance[A] {
    any =>
      // TODO
      import scala.collection.JavaConversions._
      parser.run(any.asInstanceOf[java.util.Map[String, AnyRef]].toMap)
  }

  // single field value
  def parse[A: Decoder](key: String): Parser[A] = Parser.instance[A] { m =>
    Decoder[A].run(m(key))
  }

  def parseOrElse[A: Decoder](key: String, default: A): Parser[A] = Parser.instance[A] { m =>
    Try(Decoder[A].run(m(key))).getOrElse(default)
  }

  implicit class ParserExt[A](p: Parser[A]) {
    def |(q: => Parser[A]): Parser[A] = Parser.instance[A] { m =>
      Try(p.run(m)).filter(_ != null).getOrElse(q.run(m))
    }
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

  def parse6[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder, A5:Decoder, A6:Decoder](k1: String, k2: String, k3: String, k4: String, k5: String, k6: String) = {
    (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4) |@| parse[A5](k5) |@| parse[A6](k6)).tupled
  }

  def parse7[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder, A5:Decoder, A6:Decoder, A7:Decoder](k1: String, k2: String, k3: String, k4: String, k5: String, k6: String, k7: String) = {
    (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4) |@| parse[A5](k5) |@| parse[A6](k6) |@| parse[A7](k7)).tupled
  }

  def parse8[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder, A5:Decoder, A6:Decoder, A7:Decoder, A8:Decoder](k1: String, k2: String, k3: String, k4: String, k5: String, k6: String, k7: String, k8: String) = {
    (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4) |@| parse[A5](k5) |@| parse[A6](k6) |@| parse[A7](k7) |@| parse[A8](k8)).tupled
  }

  def parse9[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder, A5:Decoder, A6:Decoder, A7:Decoder, A8:Decoder, A9:Decoder](k1: String, k2: String, k3: String, k4: String, k5: String, k6: String, k7: String, k8: String, k9: String) = {
    (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4) |@| parse[A5](k5) |@| parse[A6](k6) |@| parse[A7](k7) |@| parse[A8](k8) |@| parse[A9](k9)).tupled
  }

  def parse10[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder, A5:Decoder, A6:Decoder, A7:Decoder, A8:Decoder, A9:Decoder, A10:Decoder](k1: String, k2: String, k3: String, k4: String, k5: String, k6: String, k7: String, k8: String, k9: String, k10: String) = {
    (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4) |@| parse[A5](k5) |@| parse[A6](k6) |@| parse[A7](k7) |@| parse[A8](k8) |@| parse[A9](k9) |@| parse[A10](k10)).tupled
  }


}
