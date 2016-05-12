package org.dots42.neo4j

import org.neo4j.driver.v1.{Config => Neo4jConfig, _}
import org.neo4j.driver.v1.summary.ResultSummary

import scalaz._, Scalaz._
import scalaz.concurrent.Task

trait Utils {

  object Neo4j {

    import knobs.{Required,ClassPathResource,Config}

    def config: Task[(String, Option[(String, String)])] = {
      for {
        cfg <- knobs.loadImmutable(Required(ClassPathResource("app.conf")) :: Nil)
        url = cfg.require[String]("url")
        username = cfg.lookup[String]("username")
        password = cfg.lookup[String]("password")
      } yield (url, (username |@| password).tupled)
    }

    def driver: Driver = {
      val (url, credentials) = config.unsafePerformSync
      driver(url, credentials)
    }

    def driver(url: String, credentials: Option[(String, String)]): Driver = {

      val token: AuthToken =
        credentials.fold(AuthTokens.none) { case (username, password) => AuthTokens.basic(username, password) }

      GraphDatabase.driver(url, token, Neo4jConfig.build.withEncryptionLevel(Neo4jConfig.EncryptionLevel.NONE).toConfig)

    }

  }

  implicit def con(session: Session): Connection = Connection(session)


  implicit val resultSummaryShow = Show.shows[ResultSummary] {
    res =>
      val counters = res.counters()

      s"""
         |counters:
         |  nodesCreated: ${counters.nodesCreated()}
         |  nodesDeleted: ${counters.nodesDeleted()}
         |  relationshipsCreated: ${counters.relationshipsCreated()}
         |  relationshipsDeleted: ${counters.relationshipsDeleted()}
         |  propertiesSet: ${counters.propertiesSet()}
         |  constraintsAdded: ${counters.constraintsAdded()}
         |  constraintsRemoved: ${counters.constraintsRemoved()}
         |  containsUpdates: ${counters.containsUpdates()}
         |  labelsAdded: ${counters.labelsAdded()}
         |  labelsRemoved: ${counters.labelsRemoved()}
         |  indexesAdded: ${counters.indexesAdded()}
         |  indexesRemoved: ${counters.indexesRemoved()}
       """.stripMargin
  }


}
