package org.dots42.neo4j

import scala.util.Try
import collection.JavaConversions._
import scalaz._, Scalaz._

// Decoder[A] converts from Any to A
object Decoders {

  trait DecoderGen[I, A] {
    def run: I => A
  }

  case class Decoder[A](run: Any => A) extends DecoderGen[Any, A]

  object Decoder extends DecoderInstances {

    def apply[A:Decoder] = implicitly[Decoder[A]]

    def instance[A](run: Any => A): Decoder[A] = Decoder[A](run)

    def byCast[A]: Decoder[A] = Decoder.instance[A](_.asInstanceOf[A])

  }

  implicit val decoderFunctor = new Functor[Decoder] {
    override def map[A, B](fa: Decoder[A])(f: (A) => B): Decoder[B] = Decoder.instance[B](x => f(fa.run(x)))
  }



  // some concrete decoders
  trait DecoderInstances {

    implicit def stringDecoder: Decoder[String] = Decoder.byCast[String]
    implicit def longDecoder: Decoder[Long] = Decoder.byCast[Long]
    implicit def booleanDecoder: Decoder[Boolean] = Decoder.byCast[Boolean]
    implicit def intDecoder: Decoder[Int] = Decoder.instance[Int] {
      case x: Int => x
      case x: Long => x.toInt
      case x => x.asInstanceOf[Int]
    }
    implicit def optionalDecoder[A:Decoder]: Decoder[Option[A]] = Decoder.instance[Option[A]] { any =>
      if (any == null) None
      else Try(implicitly[Decoder[A]].run(any)).toOption.filterNot(_ == null)
    }

    implicit def listDecoder[A:Decoder]: Decoder[List[A]] = Decoder.byCast[java.util.Collection[AnyRef]] map { xs =>
      val decoder = implicitly[Decoder[A]]
      xs.toList map decoder.run
    }

  }

}
