package org.dots42.neo4j

import org.dots42.neo4j.Parsers.ParseGen
import org.neo4j.graphdb.{Node, Relationship}

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

    def decodeBy[A:Decoder, B](f: A => B): Decoder[B] = Decoder[A].map(a => f(a))

  }


  implicit lazy val decoderApplicative = new Applicative[Decoder] {

    override def point[A](a: => A): Decoder[A] = new Decoder[A] {
      override def run: (Any) => A = _ => a
    }

    override def ap[A, B](fa: => Decoder[A])(f: => Decoder[(A) => B]): Decoder[B] = new Decoder[B] {
      override def run: (Any) => B = {
        in => f.run(in)(fa.run(in))
      }
    }

  }



  // some concrete decoders
  trait DecoderInstances {

    implicit val nodeDecoder = Decoder.byCast[Node]
    implicit val relationshipDecoder = Decoder.byCast[Relationship]

    implicit def stringDecoder: Decoder[String] = Decoder.byCast[String]
    implicit def longDecoder: Decoder[Long] = Decoder.instance[Long] {
      case x: Int => x.toLong
      case x: Long => x
      case x => x.asInstanceOf[Long]
    }
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


    implicit def tuple2Decoder[A:Decoder, B:Decoder]: Decoder[(A, B)] = (Decoder[A] |@| Decoder[B]).tupled

  }

}
