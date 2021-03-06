
`ConnectionIO[A]` for Neo4j

Goals:

  - run and compose database queries as IO
  - isolate side effects
  - build queries
  - decode result with composable parsers based on decoder primitives

Types

  - `Decoder[A]`       `Any => A`
  - `Query`            together with a `Decoder[A]` yields a `ConnectionIO[A]`, `ConnectionIO[Option[A]]` or `ConnectionIO[List[A]]`
  - `ConnectionIO[A]`  usually contains runs a database query over a database connection
  - `ConnectionOp ~> Reader[Connection, ?]`   interpreter which transform a `ConnectionOp` AST to a program which runs the ConnectionOp over a give database `Connection`

Future improvements

  - improve parsers and decoders
  - use neo4j REST and/or Bolt protocol


```scala
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

  implicit val fooParser: Parser[Foo] = (parse[String]("bar") |@| parse[String]("baz")).tupled.map {
    case (bar, baz) => Foo(bar, baz)
  }

  val listFoos: ConnectionIO[List[Foo]] = query(
    """
      |match (f:Foo)
      |return
      |  f.bar as bar,
      |  f.baz as baz
    """.stripMargin).list


  val x: ConnectionIO[List[Foo]] = for {
    _ <- createFoo("hey", "ho")
    xs <- listFoos
  } yield xs

  val db = Neo4j.graphDatabaseService
  def con(db: GraphDatabaseService): Connection = Connection(db)

  val t: Task[List[Foo]] = x.transact(con(db))

  println(t.run)
  // List(Foo(hey,ho))

}
```



