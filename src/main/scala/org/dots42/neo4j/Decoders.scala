package org.dots42.neo4j

import org.dots42.neo4j.Parsers.ParseGen

import scala.util.Try
import collection.JavaConversions._
import scalaz._
import Scalaz._

// Decoder[A] converts from Any to A
object Decoders {

  trait Decoder[A] extends ParseGen[Any, A] {
    def run: Any => A
  }

  object Decoder extends DecoderInstances {

    def apply[A:Decoder]: Decoder[A] = implicitly[Decoder[A]]

    def instance[A](f: Any => A): Decoder[A] = new Decoder[A] {
      override def run: (Any) => A = any => f(any)
    }

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

    implicit def listDecoder[A:Decoder]: Decoder[List[A]] = Decoder.instance[List[A]] { xs =>

      if (xs.getClass().isArray()) {
        val decoder = implicitly[Decoder[A]]
        xs.asInstanceOf[Array[AnyRef]].toList map decoder.run
      } else {
        val decoder = implicitly[Decoder[A]]
        xs.asInstanceOf[java.util.Collection[AnyRef]].toList map decoder.run
      }

    }

  }

}
