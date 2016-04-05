package org.dots42

import org.dots42.Data._
import org.dots42.WorldTask._
import org.dots42.neo4j.Connections.{ConnectionIO, Connection}
import org.dots42.neo4j.Neo4j
import org.neo4j.graphdb.GraphDatabaseService

import scalaz._, Scalaz._
import scalaz.concurrent.Task


// computations are World => Task[A] (fused by Kleisli)
object Example2 extends App {

  // runs in a single transaction
  val p1 = DocumentModule.listCreateListDocuments("foo", Privacy.Public)

  // each task runs in its own transaction
  val p2 = for {
    x <- DocumentModule.listDocuments()
    y <- DocumentModule.createDocument("foo", Privacy.Public)
    z <- DocumentModule.listDocuments()
  } yield (x, y, z)

  val world = World(Neo4j.graphDatabaseService("data/neo4jdb"))
  val res1 = p1.run(world).run
  val res2 = p2.run(world).run

  println(res1)
  println(res2)

}

case class World(db: GraphDatabaseService) {

  def con = Connection(db)

}

object WorldTask {

  type WorldTask[A] = Kleisli[Task[?], World, A]

  def task[A](f: World => Task[A]): Kleisli[Task[?], World, A] = Kleisli[Task[?], World, A](f)

}


// each function defines a transaction boundary
object DocumentModule {

  def listDocuments() = task[List[Document]]{ world =>
    DocumentQueries.listDocuments().task(world.con)
  }

  def createDocument(name: String, privacy: Privacy) = task[Document] { world =>
    DocumentQueries.createDocument(name, privacy).task(world.con)
  }

  def listCreateListDocuments(name: String, privacy: Privacy) = task[(List[Document], Document, List[Document])] { world =>
    val p: ConnectionIO[(List[Document], Document, List[Document])] = for {
      xs1 <- DocumentQueries.listDocuments()
      doc <- DocumentQueries.createDocument(name, privacy)
      xs2 <- DocumentQueries.listDocuments()
    } yield (xs1, doc, xs2)

    p.task(world.con)
  }

}


object DocumentQueries {

  import neo4j.Connections._
  import neo4j.Queries._
  import neo4j.Parsers._

  // functor parsing
  val documentParser1: Parser[Document] = {
    parse3[String, Privacy, String]("id", "privacy", "name").map(Document.tupled)
  }

  // applicative parsing
  val documentParser: Parser[Document] = {
    (parse[String]("id") |@| parse[Privacy]("privacy") |@| parse[String]("name"))(Document.apply)
  }

  // monadic parsing
  val documentParser2: Parser[Document] = {
    for {
      id <- parse[String]("id")
      privacy <- parse[Privacy]("privacy")
      name <- parse[String]("name")
    } yield Document(id, privacy, name)
  }


  def findDocumentByText(text: String): ConnectionIO[Option[Document]] = {
    query(
      """match (d:Document)
        |where d.name =~ ".*{name}.*"
        |return
        |  d.id as id,
        |  d.privacy as privacy,
        |  d.name as name
        | """.stripMargin, Map("text" -> text)
    ).option[Document](documentParser)
  }

  def createDocument(name: String, privacy: Privacy): ConnectionIO[Document] = {
    val id = generateId

    query(
      """create (d:Document {
        |  id: {id},
        |  name: {name},
        |  privacy: {privacy}
        |})
        |return
        |  d.id as id,
        |  d.privacy as privacy,
        |  d.name as name
        | """.stripMargin, Map("id" -> id, "name" -> name, "privacy" -> privacy.shows)
    ).unique[Document](documentParser)
  }

  def listDocuments(): ConnectionIO[List[Document]] = {
    query(
      """
        |match
        |  (d:Document)
        |return
        |  d.id as id,
        |  d.privacy as privacy,
        |  d.name as name
        |""".stripMargin
    ).list[Document](documentParser)
  }

}