package org.dots42.neo4j

import org.dots42.neo4j.Decoders.Decoder
import org.neo4j.graphdb.{QueryStatistics, Result}

import scala.collection.JavaConversions._
import scala.collection.generic.CanBuildFrom
import scalaz._, Scalaz._

object Queries {

  import Connections._
  import Parsers._

  trait Query {

    def result: ConnectionIO[Result]

    def stats: ConnectionIO[QueryStatistics] = result.map(_.getQueryStatistics)

    def unit: ConnectionIO[Unit]

    def to[B, F[_]](implicit parser: Parser[B], cbf: CanBuildFrom[Nothing, B, F[B]]): ConnectionIO[F[B]]

    def point[B](b: B): ConnectionIO[B] = result.map(_ => b)

    def single[B:Decoder](key: String): ConnectionIO[B] = unique(Parser.parse[B](key))

    def unique[B](implicit parser: Parser[B]): ConnectionIO[B]

    def option[B](implicit parser: Parser[B]): ConnectionIO[Option[B]]

    def list[B](implicit parser: Parser[B]): ConnectionIO[List[B]] = to[B, List]

    def uniqueUnit[B]: ConnectionIO[Unit] = unique[Unit]
    def uniquePoint[B](b: B): ConnectionIO[B] = uniqueUnit.map(_ => b)

  }

  def query(text: String, params: Params = Map.empty): Query = new Query {

    def text1 = text

    def params1 = params

    override def result: ConnectionIO[Result] = runQuery(text, params)

    override def unit: ConnectionIO[Unit] = result map (_ => ())

    def parse1[A](parser: Parser[A], x: Map[String, AnyRef]) = {
      try {
        parser.run(x)
      } catch {
        case t: Throwable => throw new Error(s"Could not parse: $x", t)
      }
    }

    override def to[B, F[_]](implicit parser: Parser[B], cbf: CanBuildFrom[Nothing, B, F[B]]): ConnectionIO[F[B]] = {
      result map { r =>
        val builder = cbf.apply()

        while (r.hasNext) {
          builder += parse1(parser, r.next.toMap)
        }

        builder.result()
      }
    }

    override def unique[B](implicit parser: Parser[B]): ConnectionIO[B] = result map { r =>
      if (r.hasNext) {
        val x = r.next.toMap
        if (r.hasNext) throw new Error("Result set is not unique (more than one rows)")
        parse1(parser, x)
      }
      else throw new Error("Result set is not unique (empty)")
    }

    override def option[B](implicit parser: Parser[B]): ConnectionIO[Option[B]] = result map { r =>
      if (r.isEmpty) None
      else {
        val x = r.next.toMap
        try {
          Some(parse1(parser, x))
        } catch {
          case t: Throwable => None
        }
      }
    }

  }




}
