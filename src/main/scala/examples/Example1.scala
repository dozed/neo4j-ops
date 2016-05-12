package examples

import org.dots42.neo4j._

import scalaz._, Scalaz._
import scalaz.concurrent.Task

object Example1 extends App {

  case class Foo(bar: String, baz: String)

  def createFoo(bar: String, baz: String): ConnectionIO[Unit] = {
    query(
      """
        |create (f:Foo {
        |  bar: {bar},
        |  baz: {baz}
        |})
      """.stripMargin,
      Map("bar" -> bar, "baz" -> baz)
    ).unit
  }

  val fooParser: Parser[Foo] = (parse[String]("bar") |@| parse[String]("baz")).tupled.map {
    case (bar, baz) => Foo(bar, baz)
  }

  val listFoos: ConnectionIO[List[Foo]] =
    query(
      """
        |match (f:Foo)
        |return
        |  f.bar as bar,
        |  f.baz as baz
      """.stripMargin
    ).list(fooParser)


  val x: ConnectionIO[List[Foo]] = for {
    _ <- createFoo("hey", "ho")
    xs <- listFoos
  } yield xs


  val driver = Neo4j.driver

  val t: Task[List[Foo]] = x.task(driver.session)

  println(t.unsafePerformSync)
  // List(Foo(hey,ho))


}

