package org.dots42.neo4j

import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.factory.GraphDatabaseFactory

import scalaz._, Scalaz._

object Neo4j {

  def graphDatabaseService(store: String, properties: String): GraphDatabaseService = {
    new GraphDatabaseFactory()
      // .newEmbeddedDatabaseBuilder(new java.io.File(store))
      .newEmbeddedDatabaseBuilder(store)
      .loadPropertiesFromFile(properties)
      .newGraphDatabase()
  }

  def graphDatabaseService(store: String): GraphDatabaseService = {
    new GraphDatabaseFactory()
      // .newEmbeddedDatabaseBuilder(new java.io.File(store))
      .newEmbeddedDatabaseBuilder(store)
      .newGraphDatabase()
  }
}
