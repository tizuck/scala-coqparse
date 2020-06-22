package model

import model.interpTypes.notationTerm.ConstrEntryKeyGen

object parsingTypes {
  object extend {

    type SimpleConstrProdEntryKey = ConstrEntryKeyGen[Option[ProductionLevel]]
    type ConstrEntryKey = ConstrEntryKeyGen[(ProductionLevel,ProductionPosition)]

    sealed trait ProductionLevel
    case object NextLevel extends ProductionLevel
    sealed case class NumLevel(n:Int) extends ProductionLevel

    sealed trait Side
    case object Left extends Side
    case object Right extends Side

    sealed trait ProductionPosition
    sealed case class BorderProd(side:Side,assoc:Option[gramlibTypes.gramext.GAssoc]) extends ProductionPosition
    case object InternalProd extends ProductionPosition
  }
}
