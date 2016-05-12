package org.dots42.neo4j

import scala.util.Try
import scalaz._, Scalaz._

// Decoder[A] converts from Any to A
trait DecoderTypes {

  trait Decoder[A] extends ParseGen[Any, A] {
    def run: Any => A
  }

  object Decoder {

    def apply[A:Decoder]: Decoder[A] = implicitly[Decoder[A]]

    def instance[A](f: Any => A): Decoder[A] = new Decoder[A] {
      override def run: (Any) => A = any => f(any)
    }

  }

}

trait Decoders {

  implicit val decoderFunctor = new Functor[Decoder] {
    override def map[A, B](fa: Decoder[A])(f: (A) => B): Decoder[B] = Decoder.instance[B](x => f(fa.run(x)))
  }


  def decoderByCast[A]: Decoder[A] = Decoder.instance[A](_.asInstanceOf[A])

  def decoder[A](f: Any => A): Decoder[A] = Decoder.instance[A](f)


  // some concrete decoders

  implicit def stringDecoder: Decoder[String] = decoderByCast[String]
  implicit def longDecoder: Decoder[Long] = decoderByCast[Long]
  implicit def booleanDecoder: Decoder[Boolean] = decoderByCast[Boolean]
  implicit def intDecoder: Decoder[Int] = decoderByCast[Int]
  implicit def optionalDecoder[A:Decoder]: Decoder[Option[A]] = Decoder.instance[Option[A]] { any =>
    Try(Decoder[A].run(any)).toOption.filterNot(_ == null)
  }
  implicit def listDecoder[A:Decoder]: Decoder[List[A]] = decoderByCast[Array[AnyRef]] map { xs =>
    val decoder = Decoder[A]
    xs.toList map decoder.run
  }

}
