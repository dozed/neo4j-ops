package neo4jspecs

import java.util.UUID

import org.dots42.neo4j._
import org.neo4j.driver.v1.summary.ResultSummary
import org.neo4j.driver.v1.StatementResult

import collection.JavaConversions._

import org.specs2.mutable.Specification

import scalaz._, Scalaz._

class DocumentLikeQueriesSpecs extends Specification {


  // a Foo document with a Bar and a Baz
  case class Foo(id: String, bar: Bar)
  case class Bar(id: String, name: String, baz: Baz)
  case class Baz(id: String, name: String)

  def uuid: String = UUID.randomUUID().toString

  val driver = Neo4j.driver

  def createFoo(barName: String, bazName: String): ConnectionIO[(ResultSummary, Foo)] = {
    val foo =
      Foo(
        uuid,
        Bar(
          uuid, barName,
          Baz(uuid, bazName)
        )
      )

    query(
      """
        |create
        |  (f:Foo { id: {foo_id} }),
        |  (b:Bar { id: {bar_id}, name: {bar_name} }),
        |  (z:Baz { id: {baz_id}, name: {baz_name} }),
        |  (f)-[:has]->(b)<-[:has]-(z)
      """.stripMargin,
      Map(
        "foo_id" -> foo.id,
        "bar_id" -> foo.bar.id, "bar_name" -> foo.bar.name,
        "baz_id" -> foo.bar.baz.id, "baz_name" -> foo.bar.baz.name
      )
    ).summary.map(s => (s, foo))
  }

  def queryFooById(fooId: String): ConnectionIO[StatementResult] = {

    query(
      """
        |match
        |  (f:Foo)-[:has]->(b:Bar)<-[:has]-(z:Baz)
        |where
        |  f.id = {foo_id}
        |return
        |{
        |  id: f.id,
        |  bar: {
        |    id: b.id,
        |    name: b.name,
        |    baz: {
        |      id: z.id,
        |      name: z.name
        |    }
        |  }
        |} as foo
      """.stripMargin,
      Map("foo_id" -> fooId)
    ).result

  }



  "A document-like object can be created" in {

    val (res, foo) = createFoo("bar", "baz").transact(driver.session())

    println(res.shows)
    // counters:
    //   nodesCreated: 3
    //   nodesDeleted: 0
    //   relationshipsCreated: 2
    //   relationshipsDeleted: 0
    //   propertiesSet: 5
    //   constraintsAdded: 0
    //   constraintsRemoved: 0
    //   containsUpdates: true
    //   labelsAdded: 3
    //   labelsRemoved: 0
    //   indexesAdded: 0
    //   indexesRemoved: 0

    res.counters.nodesCreated should_== 3
    res.counters.relationshipsCreated should_== 2
    res.counters.propertiesSet should_== 5
    res.counters.nodesDeleted should_== 0

    foo.bar.name should_== "bar"
    foo.bar.baz.name should_== "baz"

  }

  "A document-like object can be queried" in {

    val (_, foo) = createFoo("bar", "baz").transact(driver.session)

    val rs = queryFooById(foo.id).transact(driver.session)
    val xs = rs.list().toList

    xs should haveSize(1)
    rs.consume()

    // Record<{foo: {bar: {name: "bar", baz: {name: "baz", id: "2600ebbf-8a5f-4b01-8674-c36ca8391dd4"}, id: "4387b183-49a9-4b31-be7f-e37452dcd2dc"}, id: "11f93ca7-536a-43c4-bd67-d0d7d02eb053"}}>
    val x = xs.head
    println(x)

    success

  }

  "A document-like object can be queried and parsed" in {

    implicit val bazParser: Parser[Baz] = parse2[String, String]("id", "name").map[Baz](Baz.tupled)
    implicit val barParser: Parser[Bar] = {
      for {
        x <- parse2[String, String]("id", "name")
        (id, name) = x
        baz <- parse[Baz]("baz")
      } yield Bar(id, name, baz)
    }
    implicit val fooParser: Parser[Foo] = {
      for {
        id  <- parse[String]("id")
        bar <- parse[Bar]("bar")
      } yield Foo(id, bar)
    }

    def queryFooById2(fooId: String): ConnectionIO[Option[Foo]] = {

      query(
        """
          |match
          |  (f:Foo)-[:has]->(b:Bar)<-[:has]-(z:Baz)
          |where
          |  f.id = {foo_id}
          |return
          |
          |f.id as id,
          |{
          |  id: b.id,
          |  name: b.name,
          |  baz: {
          |    id: z.id,
          |    name: z.name
          |  }
          |} as bar
          |
        """.stripMargin,
        Map("foo_id" -> fooId)
      ).option[Foo]

    }


    val (_, foo) = createFoo("bar", "baz").transact(driver.session)

    val fooOpt = queryFooById2(foo.id).transact(driver.session)

    fooOpt should beSome

  }

}
