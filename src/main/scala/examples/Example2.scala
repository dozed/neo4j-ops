package examples

import Data._

import org.dots42.neo4j._
import org.neo4j.driver.v1.Driver

import scalaz._, Scalaz._
import scalaz.concurrent.Task

object Example2 extends App {

  // runs in a single transaction
  val p1 = DocumentModule.listCreateListDocuments("foo", Privacy.Public)

  // each task runs in its own transaction
  val p2 = for {
    x <- DocumentModule.listDocuments()
    y <- DocumentModule.createDocument("foo", Privacy.Public)
    z <- DocumentModule.listDocuments()
  } yield (x, y, z)

  val world = World(Neo4j.driver)
  val res1 = p1.run(world).unsafePerformSync
  val res2 = p2.run(world).unsafePerformSync

  println(res1)
  println(res2)

}

case class World(driver: Driver) {

  def con = Connection(driver.session)

}

object Def {

  type RT[A] = Kleisli[Task[?], World, A]

  def apply[A](f: World => Task[A]): RT[A] = Kleisli[Task[?], World, A](f)

}

object Data {

  sealed trait Privacy

  object Privacy {

    case object Public extends Privacy
    case object Private extends Privacy

    def fromString(s: String): Privacy = s match {
      case "public" => Public
      case "private" => Private
    }

    def asString(p: Privacy) = p match {
      case Public => "public"
      case Private => "private"
    }

  }

  case class Document(id: String, privacyType: Privacy, name: String)

  sealed trait ErrorCode


  implicit val privacyTypeDecoder: Decoder[Privacy] = stringDecoder map Privacy.fromString

  implicit val pricacyTypeShows = Show.shows[Privacy](x => Privacy.asString(x))

}


// each function defines a transaction boundary
object DocumentModule {

  def listDocuments() = Def[List[Document]]{ world =>
    DocumentQueries.listDocuments().task(world.con)
  }

  def createDocument(name: String, privacy: Privacy) = Def[Document] { world =>
    DocumentQueries.createDocument(name, privacy).task(world.con)
  }

  def listCreateListDocuments(name: String, privacy: Privacy) = Def[(List[Document], Document, List[Document])] { world =>
    (for {
      xs1 <- DocumentQueries.listDocuments()
      doc <- DocumentQueries.createDocument(name, privacy)
      xs2 <- DocumentQueries.listDocuments()
    } yield (xs1, doc, xs2)).task(world.con)
  }

}


object DocumentQueries {

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
        | """.stripMargin,
      Map("text" -> text)
    ).option[Document](documentParser)
  }

  def createDocument(name: String, privacy: Privacy): ConnectionIO[Document] = {
    val id = java.util.UUID.randomUUID().toString

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
        | """.stripMargin,
      Map("id" -> id, "name" -> name, "privacy" -> privacy.shows)
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