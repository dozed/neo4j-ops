package org.dots42

import scalaz._, Scalaz._

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

}
