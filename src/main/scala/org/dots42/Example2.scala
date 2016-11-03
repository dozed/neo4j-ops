package org.dots42

import org.dots42.Data._
import org.dots42.neo4j.Connections.{ConnectionIO, Connection}
import org.dots42.neo4j.Neo4j
import org.neo4j.graphdb.GraphDatabaseService

import scalaz._, Scalaz._


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
  val res1 = p1.run(world.con)
  val res2 = p2.run(world.con)

  println(res1)
  println(res2)

}

case class World(db: GraphDatabaseService) {

  def con = Connection(db)

}


// each function defines a transaction boundary
object DocumentModule {

  def listDocuments() = {
    DocumentQueries.listDocuments().transact
  }

  def createDocument(name: String, privacy: Privacy) = {
    DocumentQueries.createDocument(name, privacy).transact
  }

  def listCreateListDocuments(name: String, privacy: Privacy) = {
    val p: ConnectionIO[(List[Document], Document, List[Document])] = for {
      xs1 <- DocumentQueries.listDocuments()
      doc <- DocumentQueries.createDocument(name, privacy)
      xs2 <- DocumentQueries.listDocuments()
    } yield (xs1, doc, xs2)

    p.transact
  }

}


object DocumentQueries {

  import neo4j.Connections._
  import neo4j.Queries._
  import neo4j.Parsers._

  // functor parsing
  val documentParser1: Parser[Document] = {
    Parser.tuple3[String, Privacy, String]("id", "privacy", "name").map(Document.tupled)
  }

  // applicative parsing
  val documentParser: Parser[Document] = {
    (Parser.parse[String]("id") |@| Parser.parse[Privacy]("privacy") |@| Parser.parse[String]("name"))(Document.apply)
  }

  // monadic parsing
  val documentParser2: Parser[Document] = {
    for {
      id <- Parser.parse[String]("id")
      privacy <- Parser.parse[Privacy]("privacy")
      name <- Parser.parse[String]("name")
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