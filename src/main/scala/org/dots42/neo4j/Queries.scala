package org.dots42.neo4j

import org.neo4j.graphdb.{QueryStatistics, Result}

import scala.collection.JavaConversions._
import scala.collection.generic.CanBuildFrom
import scala.util.Try
import scalaz._, Scalaz._

object Queries {

  import Connections._
  import Parsers._

  trait Query {

    def to[B, F[_]](implicit parser: Parser[B], cbf: CanBuildFrom[Nothing, B, F[B]]): ConnectionIO[F[B]]

    def unique[B](implicit parser: Parser[B]): ConnectionIO[B]

    def option[B](implicit parser: Parser[B]): ConnectionIO[Option[B]]

    def list[B](implicit parser: Parser[B]): ConnectionIO[List[B]] = to[B, List]

    def result: ConnectionIO[Result]

    def unit: ConnectionIO[Unit]

  }

  // TODO
  // case class ResultChecker(p: QueryStatistics => Boolean)

  def query(text: String, params: Params = Map.empty): Query = new Query {

    def text1 = text

    def params1 = params

    override def result: ConnectionIO[Result] = runQuery(text, params)

    override def unit: ConnectionIO[Unit] = result map (_ => ())

    override def to[B, F[_]](implicit parser: Parser[B], cbf: CanBuildFrom[Nothing, B, F[B]]): ConnectionIO[F[B]] = {
      result map { r =>
        val builder = cbf.apply()

        while (r.hasNext) {
          val x = r.next.toMap
          Try {
            builder += parser.run(x)
          } recover {
            case t => println(f"could not parse: $x"); t.printStackTrace()
          }
        }

        builder.result()
      }
    }

    override def unique[B](implicit parser: Parser[B]): ConnectionIO[B] = result map { r =>
      if (r.hasNext) {
        val b = parser.run(r.next.toMap)
        if (r.hasNext) throw new Error("Result set is not unique (more than one rows)")
        b
      }
      else throw new Error("Result set is not unique (empty)")
    }

    override def option[B](implicit parser: Parser[B]): ConnectionIO[Option[B]] = result map { r =>
      if (r.isEmpty) None
      else {
        val b = Try(parser.run(r.next.toMap)).toOption
        if (r.hasNext) throw new Error("Result set is not unique (more than one rows)")
        b
      }
    }

  }




}
