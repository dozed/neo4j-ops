package org.dots42.neo4j

import scala.util.Try
import scalaz._, Scalaz._

// Decoder[A] converts from Any to A
object Decoders {

  trait DecoderGen[I, A] {
    def run: I => A
  }

  case class Decoder[A](run: Any => A) extends DecoderGen[Any, A]

  implicit val decoderFunctor = new Functor[Decoder] {
    override def map[A, B](fa: Decoder[A])(f: (A) => B): Decoder[B] = Decoder[B](x => f(fa.run(x)))
  }


  def decoderByCast[A]: Decoder[A] = Decoder[A](_.asInstanceOf[A])

  def decoder[A](f: Any => A): Decoder[A] = Decoder[A](f)


  // some concrete decoders

  implicit def stringDecoder: Decoder[String] = decoderByCast[String]
  implicit def longDecoder: Decoder[Long] = decoderByCast[Long]
  implicit def booleanDecoder: Decoder[Boolean] = decoderByCast[Boolean]
  implicit def intDecoder: Decoder[Int] = decoderByCast[Int]
  implicit def optionalDecoder[A:Decoder]: Decoder[Option[A]] = Decoder { any =>
    if (any == null) None
    else Try(implicitly[Decoder[A]].run(any)).toOption.filterNot(_ == null)
  }
  implicit def listDecoder[A:Decoder]: Decoder[List[A]] = decoderByCast[Array[AnyRef]] map { xs =>
    val decoder = implicitly[Decoder[A]]
    xs.toList map decoder.run
  }

}
