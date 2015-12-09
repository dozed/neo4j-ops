package org.dots42

import org.dots42.Data._
import org.dots42.WorldTask._
import org.dots42.neo4j.Connections.{ConnectionIO, Connection}
import org.dots42.neo4j.Neo4j
import org.neo4j.graphdb.GraphDatabaseService

import scalaz._, Scalaz._
import scalaz.concurrent.Task


case class World(db: GraphDatabaseService) {

  def con = Connection(db)

}

object WorldTask {

  type WorldTask[A] = Kleisli[Task[?], World, A]

  def task[A](f: World => Task[A]): Kleisli[Task[?], World, A] = Kleisli[Task[?], World, A](f)

}


object MyApp extends App {

  val world = World(Neo4j.graphDatabaseService)

  val op = Operation.CreateDocument("foo", PrivacyType.Public)

  // runs in a single transaction
  val res = Api.listCreateListDocuments(op).run(world).run
  println(res)

  // runs 3 transactions
  val p2 = for {
    x <- Api.listDocuments()
    y <- Api.createDocument(op)
    z <- Api.listDocuments()
  } yield (x, y, z)

  val res2 = p2.run(world).run

}


object Api {

  def listDocuments() = task[List[Document]]{ world =>
    MyQueries.listDocuments().transact(world.con)
  }

  def createDocument(op: Operation.CreateDocument) = task[Document] { world =>
    MyQueries.createDocument(op.name, op.privacy).transact(world.con)
  }

  def listCreateListDocuments(op: Operation.CreateDocument) = task[(List[Document], Document, List[Document])] { world =>
    val p: ConnectionIO[(List[Document], Document, List[Document])] = for {
      xs1 <- MyQueries.listDocuments()
      doc <- MyQueries.createDocument(op.name, op.privacy)
      xs2 <- MyQueries.listDocuments()
    } yield (xs1, doc, xs2)

    p.transact(world.con)
  }

}


object MyQueries {

  import neo4j.Connections._
  import neo4j.Queries._
  import neo4j.Parsers._

  val documentParser: Parser[Document] =
    (parse[String]("id") |@|
      parse[PrivacyType]("privacyType") |@|
      parse[String]("name")).tupled.map {
      case (id, privacyType, name) => Document(id, privacyType, name)
    }

  def findDocumentByText(text: String): ConnectionIO[Option[Document]] = {
    query[Document](
      """match (d:Document)
        |where d.name =~ ".*{name}.*"
        |return
        |  d.id as id,
        |  d.privacyType as privacyType,
        |  d.name as name
        | """.stripMargin, Map("text" -> text))(documentParser).option
  }

  def createDocument(name: String, privacyType: PrivacyType): ConnectionIO[Document] = {
    val id = generateId

    query[Document](
      """create (d:Document {
        |  id: {id},
        |  name: {name},
        |  privacyType: {privacyType}
        |})
        |return
        |  d.id as id,
        |  d.privacyType as privacyType,
        |  d.name as name
        | """.stripMargin, Map("id" -> id, "name" -> name, "privacyType" -> privacyType.shows))(documentParser).unique
  }

  def listDocuments(): ConnectionIO[List[Document]] = {
    query[Document](
      """
        |match
        |  (d:Document)
        |return
        |  d.id as id,
        |  d.privacyType as privacyType,
        |  d.name as name
        |""".stripMargin)(documentParser).list
  }

}