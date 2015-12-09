package org.dots42

import org.dots42.Data._
import org.dots42.neo4j.Connections.Connection
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.factory.GraphDatabaseFactory

import scalaz._, Scalaz._
import scalaz.concurrent.Task

object MyApp extends App {

  val p = for {
    xs1 <- Api.listDocuments()
    doc <- Api.createDocument(Operation.CreateDocument("foo", PrivacyType.Public))
    xs2 <- Api.listDocuments()
  } yield (xs1, doc, xs2)

  val res = p.run
  println(res)

}


object Api {

  def graphDatabaseService: GraphDatabaseService = {
    new GraphDatabaseFactory()
      .newEmbeddedDatabaseBuilder(new java.io.File("data/neo4j-2.3.1/data"))
      .newGraphDatabase()

    // .loadPropertiesFromFile("data/neo4j-2.3.1/neo4j.properties")
  }

  lazy val db = graphDatabaseService

  def con = Connection(db)

  def listDocuments(): Task[List[Document]] = MyQueries.listDocuments().transact(con)

  def createDocument(op: Operation.CreateDocument): Task[Document] = MyQueries.createDocument(op.name, op.privacy).transact(con)

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