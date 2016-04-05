package org.dots42.neo4j

import org.dots42.neo4j.Connections._
import org.dots42.neo4j.Parsers._
import org.dots42.neo4j.Queries._
import org.neo4j.graphdb.GraphDatabaseService
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAfterAll

import scalaz._, Scalaz._
import scalaz.concurrent.Task

object TestDomainAndQueries {

  case class Foo(bar: String, baz: String)

  def createFoo(bar: String, baz: String): ConnectionIO[Foo] = {
    query(
      """
        |create (f:Foo {
        |  bar: {bar},
        |  baz: {baz}
        |})
      """.stripMargin, Map("bar" -> bar, "baz" -> baz)
    ).unique(Foo(bar, baz).point[Parser])
  }

  val queryFoos: ConnectionIO[List[Foo]] = query(
    """
      |match (f:Foo)
      |return
      |  f.bar as bar,
      |  f.baz as baz
    """.stripMargin).list(parse2[String, String]("bar", "baz").map(Foo.tupled))

}

class Neo4jSpecs extends Specification with BeforeAfterAll {

  import TestDomainAndQueries._

  val db: GraphDatabaseService = Neo4j.graphDatabaseService("data/neo4jdb")

  override def beforeAll(): Unit = {}

  override def afterAll(): Unit = {
    db.shutdown()
  }


  "it should be possible to create and query foos" in {

    val createAndQuery: ConnectionIO[(Foo, List[Foo])] = for {
      foo <- createFoo("hey", "ho")
      xs <- queryFoos
    } yield (foo, xs)

    val task: Task[(Foo, List[Foo])] = createAndQuery.task(Connection(db))

    val (foo, foos): (Foo, List[Foo]) = task.run

    foos should not(beEmpty)

    foos should contain(foo)

  }


}
