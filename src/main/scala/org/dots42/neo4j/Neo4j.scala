package org.dots42.neo4j

import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.factory.GraphDatabaseFactory

import scalaz._, Scalaz._

object Neo4j {

  def graphDatabaseService: GraphDatabaseService = {
    new GraphDatabaseFactory()
      .newEmbeddedDatabaseBuilder(new java.io.File("data/neo4j-2.3.1/data"))
      .newGraphDatabase()

    // .loadPropertiesFromFile("data/neo4j-2.3.1/neo4j.properties")
  }

}
