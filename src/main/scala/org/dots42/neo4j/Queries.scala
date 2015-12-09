package org.dots42.neo4j

import org.neo4j.graphdb.{QueryStatistics, Result}

import scala.collection.JavaConversions._
import scala.collection.generic.CanBuildFrom
import scala.util.Try
import scalaz.Scalaz._
import scalaz._

object Queries {

  import Connections._
  import Parsers._

  // ConnectionIO[A] combinators
  trait Query[B] {

    def to[F[_]](implicit cbf: CanBuildFrom[Nothing, B, F[B]]): ConnectionIO[F[B]]

    def unique: ConnectionIO[B]

    def option: ConnectionIO[Option[B]]

    def list: ConnectionIO[List[B]] = to[List]

    def result: ConnectionIO[Result]

    def map[C](f: B => C): Query[C]

  }

  case class ResultChecker(p: QueryStatistics => Boolean)

  //  implicit val queryFunctor = new Functor[Query] {
  //    override def map[A, B](fa: Query[A])(f: (A) => B): Query[B] = ???
  //  }


  // takes a Parser, should also take a result checker
  def query[B:Parser](text: String, params: Params = Map.empty): Query[B] = new Query[B] {

    lazy val parser = implicitly[Parser[B]]

    override def result: ConnectionIO[Result] = runQuery(text, params)

    override def to[F[_]](implicit cbf: CanBuildFrom[Nothing, B, F[B]]): ConnectionIO[F[B]] = {
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

    override def unique: ConnectionIO[B] = result map { r =>
      if (r.hasNext) {
        val b = parser.run(r.next.toMap)
        if (r.hasNext) throw new Error("Result set is not unique (more than one rows)")
        b
      }
      else throw new Error("Result set is not unique (empty)")
    }

    override def option: ConnectionIO[Option[B]] = result map { r =>
      if (r.isEmpty) None
      else {
        val b = Try(parser.run(r.next.toMap)).toOption
        if (r.hasNext) throw new Error("Result set is not unique (more than one rows)")
        b
      }
    }

    override def map[C](f: (B) => C): Query[C] = Queries.query[C](text, params)(parser.map(f))

  }


  def checkedQuery[B:Parser](text: String, params: Params = Map.empty)(check: ResultChecker): Query[B] = new Query[B] {

    lazy val parser = implicitly[Parser[B]]

    override def result: ConnectionIO[Result] = runQuery(text, params) map { r => check.p(r.getQueryStatistics); r }

    override def to[F[_]](implicit cbf: CanBuildFrom[Nothing, B, F[B]]): ConnectionIO[F[B]] = {
      result map { r =>
        val builder = cbf.apply()

        while (r.hasNext) {
          builder += parser.run(r.next.toMap)
        }

        builder.result()
      }
    }

    override def unique: ConnectionIO[B] = result map { r =>
      if (r.hasNext) {
        val b = parser.run(r.next.toMap)
        if (r.hasNext) throw new Error("Result set is not unique (more than one rows)")
        b
      }
      else throw new Error("Result set is not unique (empty)")
    }

    override def option: ConnectionIO[Option[B]] = result map { r =>
      if (r.isEmpty) None
      else {
        val b = Try(parser.run(r.next.toMap)).toOption
        if (r.hasNext) throw new Error("Result set is not unique (more than one rows)")
        b
      }
    }


    override def map[C](f: (B) => C): Query[C] = Queries.query[C](text, params)(parser.map(f))

  }


}
