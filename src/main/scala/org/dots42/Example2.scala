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

  val world = World(Neo4j.graphDatabaseService)
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
    DocumentQueries.listDocuments().transact(world.con)
  }

  def createDocument(name: String, privacy: Privacy) = task[Document] { world =>
    DocumentQueries.createDocument(name, privacy).transact(world.con)
  }

  def listCreateListDocuments(name: String, privacy: Privacy) = task[(List[Document], Document, List[Document])] { world =>
    val p: ConnectionIO[(List[Document], Document, List[Document])] = for {
      xs1 <- DocumentQueries.listDocuments()
      doc <- DocumentQueries.createDocument(name, privacy)
      xs2 <- DocumentQueries.listDocuments()
    } yield (xs1, doc, xs2)

    p.transact(world.con)
  }

}


object DocumentQueries {

  import neo4j.Connections._
  import neo4j.Queries._
  import neo4j.Parsers._

  val documentParser: Parser[Document] = {
    (parse[String]("id") |@| parse[Privacy]("privacy") |@| parse[String]("name")).tupled.map {
      case (id, privacy, name) => Document(id, privacy, name)
    }
  }

  def findDocumentByText(text: String): ConnectionIO[Option[Document]] = {
    query[Document](
      """match (d:Document)
        |where d.name =~ ".*{name}.*"
        |return
        |  d.id as id,
        |  d.privacy as privacy,
        |  d.name as name
        | """.stripMargin, Map("text" -> text))(documentParser).option
  }

  def createDocument(name: String, privacy: Privacy): ConnectionIO[Document] = {
    val id = generateId

    query[Document](
      """create (d:Document {
        |  id: {id},
        |  name: {name},
        |  privacy: {privacy}
        |})
        |return
        |  d.id as id,
        |  d.privacy as privacy,
        |  d.name as name
        | """.stripMargin, Map("id" -> id, "name" -> name, "privacy" -> privacy.shows))(documentParser).unique
  }

  def listDocuments(): ConnectionIO[List[Document]] = {
    query[Document](
      """
        |match
        |  (d:Document)
        |return
        |  d.id as id,
        |  d.privacy as privacy,
        |  d.name as name
        |""".stripMargin)(documentParser).list
  }

}