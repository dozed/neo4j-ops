package org.dots42.neo4j

import org.dots42.Data.Privacy

import scalaz._, Scalaz._

object Parsers {

  import Connections._

  // optics?
  trait ParseGen[I, A]

  case class Parser[A](run: ResultItem => A) extends ParseGen[ResultItem, A]

  case class Decoder[A](run: AnyRef => A) extends ParseGen[ResultItem, A]

  // traversal + parser
  def parse[A: Decoder](key: String): Parser[A] = Parser[A] { m =>
    implicitly[Decoder[A]].run(m(key))
  }


  implicit val parserApplicative = new Applicative[Parser] {
    override def point[A](a: => A): Parser[A] = Parser[A](_ => a)

    // apply a function, e.g. collect of multiple Applicatives to tuples
    override def ap[A, B](fa: => Parser[A])(f: => Parser[(A) => B]): Parser[B] = Parser[B](m => f.run(m)(fa.run(m)))
  }

  implicit val decoderFunctor = new Functor[Decoder] {
    override def map[A, B](fa: Decoder[A])(f: (A) => B): Decoder[B] = Decoder[B](x => f(fa.run(x)))
  }

  def decoderByCast[A]: Decoder[A] = Decoder[A](_.asInstanceOf[A])

  def decoder[A](f: AnyRef => A): Decoder[A] = Decoder[A](f)



  // some concrete parsers/decoders

  implicit def stringDecoder: Decoder[String] = decoderByCast[String]

  implicit val structureTypeDecoder: Decoder[Privacy] = stringDecoder map Privacy.fromString

  implicit val resultItemParser = Parser[ResultItem](identity)

  // parse first value of set
  implicit val stringParser = Parser[String](m => m.values.head.asInstanceOf[String])


}
