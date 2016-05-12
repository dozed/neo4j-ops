package org.dots42

import scalaz._, Scalaz._

package object neo4j
  extends ConnectionTypes
  with Connections
  with DecoderTypes
  with Decoders
  with ParserTypes
  with Parsers
  with Queries
  with Utils
