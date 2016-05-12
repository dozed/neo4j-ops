package neo4jspecs

import org.dots42.neo4j._
import org.neo4j.driver.v1._
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAfterAll

import scalaz._, Scalaz._

class SimpleSpecs extends Specification with BeforeAfterAll {

  val driver: Driver = Neo4j.driver

  override def beforeAll(): Unit = {}

  override def afterAll(): Unit = {
    driver.close()
  }



  case class Foo(bar: String, baz: String)

  object Queries {

    implicit val fooParser = parse2[String, String]("bar", "baz").map(Foo.tupled)

    def createFoo(bar: String, baz: String): ConnectionIO[Foo] =
      query(
        """
          |create (f:Foo {
          |  bar: {bar},
          |  baz: {baz}
          |})
          |return 1
        """.stripMargin, Map("bar" -> bar, "baz" -> baz)
      ).point(Foo(bar, baz))

    val queryFoos: ConnectionIO[List[Foo]] =
      query(
        """
          |match (f:Foo)
          |return
          |  f.bar as bar,
          |  f.baz as baz
        """.stripMargin
      ).list[Foo]

  }

  import Queries._

  "A Foo can be created and queried" in {

    val createAndQuery: ConnectionIO[(Foo, List[Foo])] = for {
      foo <- createFoo("hey", "ho")
      xs <- queryFoos
    } yield (foo, xs)

    val (foo, foos): (Foo, List[Foo]) = createAndQuery.transact(Connection(driver.session()))

    foos should not(beEmpty)

    foos should contain(foo)

  }


}
