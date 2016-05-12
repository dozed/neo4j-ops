package org.dots42.neo4j

import org.neo4j.driver.v1.{Record, StatementResult}
import org.neo4j.driver.v1.summary.ResultSummary

import scala.collection.JavaConversions._
import scala.collection.generic.CanBuildFrom
import scala.util.Try
import scalaz._
import Scalaz._

trait Queries {

  trait Query { query =>

    def result: ConnectionIO[StatementResult]

    def summary: ConnectionIO[ResultSummary]

    def unit: ConnectionIO[Unit]

    def to[B:Parser, F[_]](implicit cbf: CanBuildFrom[Nothing, B, F[B]]): ConnectionIO[F[B]]

    def point[B](b: B): ConnectionIO[B]

    def unique[B:Parser]: ConnectionIO[B]

    def option[B:Parser]: ConnectionIO[Option[B]]

    def list[B:Parser]: ConnectionIO[List[B]] = to[B, List]

  }


  def query(text: String, params: Params = Map.empty): Query = new Query {

    override def result: ConnectionIO[StatementResult] = runQuery(text, params)

    override def summary: ConnectionIO[ResultSummary] = result map (_.consume())

    override def unit: ConnectionIO[Unit] = result map (_ => ())

    override def to[B:Parser, F[_]](implicit cbf: CanBuildFrom[Nothing, B, F[B]]): ConnectionIO[F[B]] = {
      result map { rs =>
        val builder = cbf.apply()
        val parser = Parser[B]

        while (rs.hasNext) {
          val r = rs.next.asMap.toMap
          Try {
            builder += parser.run(r)
          } recover {
            case t => println(f"could not parse: $r"); t.printStackTrace()
          }
        }

        builder.result()
      }
    }

    override def point[B](b: B): ConnectionIO[B] = b.point[ConnectionIO]

    override def unique[B:Parser]: ConnectionIO[B] = result map { rs =>
      if (rs.hasNext) {
        val r = rs.next.asMap.toMap
        val b = Parser[B].run(r)
        if (rs.hasNext) sys.error("Result set is not unique (more than one rows)")
        b
      }
      else throw new Error("Result set is not unique (empty)")
    }

    override def option[B:Parser]: ConnectionIO[Option[B]] = result map { rs =>
      if (rs.isEmpty) None
      else {
        val r = rs.next.asMap.toMap
        val b = Try(Parser[B].run(r)).cata(_.some, t => { println(f"could not parse: $r"); t.printStackTrace(); none })
        if (rs.hasNext) sys.error("Result set is not unique (more than one rows)")
        b
      }
    }

  }




}
