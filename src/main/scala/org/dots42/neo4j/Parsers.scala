package org.dots42.neo4j

import scala.util.Try
import scalaz._, Scalaz._
import collection.JavaConversions._


object Parsers {

  import Connections._
  import Decoders._

  trait ParseGen[I, A]

  trait Parser[A] extends ParseGen[ResultItem, A] { self =>

    // TODO ParseResult
    def run: ResultItem => A

    def attempt(r: ResultItem): Throwable \/ A = {
      try {
        run(r).right
      } catch {
        case t: Throwable => t.left
      }
    }

    def upCast[AA >: A]: Parser[AA] = Parser.instance[AA](run)

    def orElse[AA >: A](q: => Parser[AA]): Parser[AA] = Parser.instance[AA] { m =>
      Try(run(m)).filter(_ != null).getOrElse(q.run(m))
    }

    def |(q: => Parser[A]): Parser[A] = orElse(q)

    def down(key: String): Parser[A] = Parser.instance[A] { m =>
      self.run(toResultItem(m(key)))
    }

    def toDecoder: Decoder[A] = Decoder.instance[A] {
      any =>
        self.run(toResultItem(any))
    }

    def toOption: Parser[Option[A]] = new Parser[Option[A]] {
      override def run = res => Try(self.run(res)).toOption
    }

  }

  def toResultItem(a: Any): ResultItem = {
    a match {
      case x:java.util.Map[String, Any] => x.toMap
      case x:Map[String, Any] => x
    }
  }

  object Parser extends ParserInstances with TupleParsers {

    def apply[A:Parser]: Parser[A] = implicitly[Parser[A]]

    def const[A](a: A) = Parser.instance[A](_ => a)

    def pure[A](a: A) = Parser.instance[A](_ => a)

    def instance[A](f: ResultItem => A): Parser[A] = new Parser[A] {
      override def run: (ResultItem) => A = rs => f(rs)
    }

    def parse[A: Decoder](key: String): Parser[A] = Parser.instance[A](m => Decoder[A].run(m(key)))

    def parseOpt[A: Decoder](key: String): Parser[Option[A]] =
      Parser.instance[Option[A]](
        m =>
          // key can be missing, or decoder can fail
          Try(Decoder[A].run(m(key))).filter(_ != null).toOption
      )

    def parseOrElse[A: Decoder](key: String, default: A): Parser[A] = Parser.instance[A] { m =>
      parseOpt(key).run(m).getOrElse(default)
    }

    def down(key: String): Cursor = Cursor(List(key))

    def optional[A: Parser]: Parser[Option[A]] = Parser.instance[Option[A]] { m =>
      Try(Parser[A].run(m)).toOption
    }

    def list[A:Parser](key: String): Parser[List[A]] =
      Parser.instance[List[A]] {
        res =>
          Decoder.listDecoder[A](Parser[A].toDecoder).run(res(key))
      }

  }

  case class Cursor(history: List[String]) {

    def down(key: String): Cursor = Cursor(key :: history)

    def as[A:Parser]: Parser[A] = Parser.instance[A] { m =>
      Parser[A].run {
        history.reverse.foldLeft(m) { case (m, key) =>
          toResultItem(m(key))
        }
      }
    }

  }

  implicit val parserMonad = new Monad[Parser] {
    override def point[A](a: => A): Parser[A] = Parser.instance[A](_ => a)

    override def bind[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] = Parser.instance[B] { res =>
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

  implicit def parserDecoder[A](implicit parser: Parser[A]): Decoder[A] = parser.toDecoder


  // TODO remove
  def parseOrElse[A: Decoder](key: String, default: A): Parser[A] = Parser.instance[A] { m =>
    Try(Decoder[A].run(m(key))).getOrElse(default)
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

    def tuple5[A1:Decoder, A2:Decoder, A3:Decoder, A4:Decoder, A5:Decoder](k1: String, k2: String, k3: String, k4: String, k5: String) = {
      (parse[A1](k1) |@| parse[A2](k2) |@| parse[A3](k3) |@| parse[A4](k4) |@| parse[A5](k5)).tupled
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
