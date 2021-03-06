package org.dots42.neo4j

import org.dots42.neo4j.Decoders.Decoder

import scala.collection.JavaConversions._
import scala.collection.generic.CanBuildFrom
import scalaz._, Scalaz._

object Queries {

  import Connections._
  import Parsers._

  trait Query {

    def result: ConnectionIO[Result]

    def require(p: QueryStatistics => Boolean): ConnectionIO[Result] = {
      result.map { res =>
        if (p(res.getQueryStatistics)) res
        else throw new RuntimeException(s"result constraint: ${res.getQueryStatistics.shows}")
      }
    }

    def stats: ConnectionIO[QueryStatistics] = result.map(_.getQueryStatistics)

    def unit: ConnectionIO[Unit]

    def point[A](a: A): ConnectionIO[A] = result.map(_ => a)

    def unique[A:Parser]: ConnectionIO[A]

    def option[A:Parser]: ConnectionIO[Option[A]]

    def list[A:Parser]: ConnectionIO[List[A]] = to[A, List]

    def to[A:Parser, F[_]](implicit cbf: CanBuildFrom[Nothing, A, F[A]]): ConnectionIO[F[A]]

    def single[A:Decoder](key: String): ConnectionIO[A] = unique(Parser.parse[A](key))

    def tuple2[A:Decoder, B:Decoder](key1: String, key2: String): ConnectionIO[(A, B)] = unique(Parser.tuple2[A, B](key1, key2))

    def tuple2Opt[A:Decoder, B:Decoder](key1: String, key2: String): ConnectionIO[Option[(A, B)]] = option(Parser.tuple2[A, B](key1, key2))

    def singleList[A:Decoder](key: String): ConnectionIO[List[A]] = list(Parser.parse[A](key))

    def uniqueUnit: ConnectionIO[Unit] = unique[Unit]

    def uniquePoint[A](a: A): ConnectionIO[A] = uniqueUnit.map(_ => a)

  }

  def query(text: String, params: Params = Map.empty): Query = new Query {

    override def result: ConnectionIO[Result] = runQuery(text, params)

    override def unit: ConnectionIO[Unit] = result map (_ => ())

    def parse1[A](item: Map[String, AnyRef])(implicit parser: Parser[A]) = {
      try {
        parser.run(item)
      } catch {
        case t: Throwable => throw new RuntimeException(s"Could not parse: $item", t)
      }
    }

    override def to[A:Parser, F[_]](implicit cbf: CanBuildFrom[Nothing, A, F[A]]): ConnectionIO[F[A]] = {
      result map { r =>
        val builder = cbf.apply()

        while (r.hasNext) {
          builder += parse1(r.next.toMap)
        }

        builder.result()
      }
    }

    override def unique[A:Parser]: ConnectionIO[A] = result map { r =>
      if (r.hasNext) {
        val x = r.next.toMap
        if (r.hasNext) throw new RuntimeException("Result set is not unique (more than one rows)")
        parse1(x)
      }
      else throw new RuntimeException("Result set is not unique (empty)")
    }

    override def option[A:Parser]: ConnectionIO[Option[A]] = result map { r =>
      if (r.isEmpty) None
      else {
        val x = r.next.toMap
        try {
          Some(parse1(x))
        } catch {
          case t: Throwable => None
        }
      }
    }

  }


  implicit lazy val queryStatisticsShow = Show.shows[QueryStatistics] { s =>

    s"QueryStatistics( nc: ${s.getNodesCreated}, nd: ${s.getNodesDeleted}, rc: ${s.getRelationshipsCreated}, rd: ${s.getRelationshipsDeleted}, ps: ${s.getPropertiesSet} )"

  }


}
