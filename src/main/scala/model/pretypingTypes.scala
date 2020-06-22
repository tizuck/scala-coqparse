package model

import model.kernelTypes.id
import model.libTypes.acyclicGraph.ConstraintType
import model.pretypingTypes.globTerm.global.SortGen
import model.pretypingTypes.globTerm.universe.Kind

object pretypingTypes {

  /**
    * Superclass for types of Casts. Casts are related to the following operators found in
    * [[parsebackParse.termParse.OprConstr]]:
    *
    * {{{
    *   t <: t'
    *   t <<: t'
    *   t : t'
    *   t :> }}}
 *
    * @tparam A The Type of the Casted expression
    */
  sealed trait CastType[+A]

  /**
    * Cast expressed by ":" found in [[parsebackParse.termParse.OprConstr]]
 *
    * @param a The casted expression
    * @tparam A The Type of the Casted expression
    */
  sealed case class CastConv[A](a:A) extends CastType[A]

  /**
    * Cast expressed by "<:" found in [[parsebackParse.termParse.OprConstr]]
 *
    * @param a The casted expression
    * @tparam A The Type of the Casted expression
    */
  sealed case class CastVM [A](a:A) extends CastType[A]

  /**
    * Cast expressed by ":>" found in [[parsebackParse.termParse.OprConstr]]
    */
  case object CastCoerce extends CastType[Nothing]

  /**
    * Cast expressed by "<<:" found in [[parsebackParse.termParse.OprConstr]]
 *
    * @param a The casted expression
    * @tparam A The Type of the Casted expression
    */
  sealed case class CastNative[A](a:A) extends CastType[A]

  object pattern {
    //TODO: Is this type really necessary
    type PatVar = id.T
  }

  object globTerm {

    type ExistentialName = id.T
    object global {

      sealed trait SortGen[+T]

      case object Prop extends SortGen[Nothing]

      case object Set extends SortGen[Nothing]

      sealed case class Type[T](t: T) extends SortGen[T]
    }

    object universe {

      sealed trait Kind[+T]

      case object Anonymous extends Kind[Nothing]

      case object Unknown extends Kind[Nothing]

      sealed case class Named [T] (t:T) extends Kind[T]

    }

    type LevelInfo = Kind[libraryTypes.libNames.Qualid]

    type GlobLevel = SortGen[LevelInfo]

    type SortInfo =  List[Option[(libraryTypes.libNames.Qualid,Int)]]

    type GlobSort = SortGen[SortInfo]

    type GlobConstraint = (GlobLevel,ConstraintType,GlobLevel)
  }

  object locus {

    sealed trait OrVar [+A]
    sealed case class ArgArg[A](a:A) extends OrVar[A]
    sealed case class ArgVar(i:kernelTypes.id.T) extends OrVar[Nothing]

    sealed trait OccurrencesGen [+A]
    case object AllOccurrences extends OccurrencesGen[Nothing]
    sealed case class AllOccurrencesBut [A] (as:List[A]) extends OccurrencesGen[A]
    case object NoOccurrences extends OccurrencesGen[Nothing]
    sealed case class OnlyOccurrences[A](as:List[A]) extends OccurrencesGen[A]

    type OccurencesExpr = OccurrencesGen[OrVar[Int]]
    type WithOccurences [A] = (OccurencesExpr,A)
  }

  object typeclasses {
    sealed case class HintInfoGen[+A](hintPriority:Option[Int],hintPattern:Option[A])
  }
}
