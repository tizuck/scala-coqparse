package model

object gramlibTypes {
  object gramext {
    sealed trait GAssoc
    case object NonA extends GAssoc
    case object RightA extends GAssoc
    case object LeftA extends GAssoc
  }
}
