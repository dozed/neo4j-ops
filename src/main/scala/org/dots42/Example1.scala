package org.dots42

import org.dots42.neo4j.Neo4j

import scalaz._, Scalaz._

object Example1 extends App {

  import neo4j.Connections._
  import neo4j.Queries._
  import neo4j.Parsers._

  case class Foo(bar: String, baz: String)

  def createFoo(bar: String, baz: String): ConnectionIO[Result] = {
    query(
      """
        |create (f:Foo {
        |  bar: {bar},
        |  baz: {baz}
        |})
      """.stripMargin, Map("bar" -> bar, "baz" -> baz)).result
  }

  val fooParser: Parser[Foo] = (Parser.parse[String]("bar") |@| Parser.parse[String]("baz")).tupled.map {
    case (bar, baz) => Foo(bar, baz)
  }

  val listFoos: ConnectionIO[List[Foo]] = query(
    """
      |match (f:Foo)
      |return
      |  f.bar as bar,
      |  f.baz as baz
    """.stripMargin).list(fooParser)


  val x: ConnectionIO[List[Foo]] = for {
    _ <- createFoo("hey", "ho")
    xs <- listFoos
  } yield xs

  val db = Neo4j.graphDatabaseService("data/neo4jdb")
  def con(db: GraphDatabaseService): Connection = Connection(db)

  val t: Reader[Connection, List[Foo]] = x.transact

  println(t.run(con(db)))
  // List(Foo(hey,ho))


}

