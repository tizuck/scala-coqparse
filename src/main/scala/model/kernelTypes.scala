package model

object kernelTypes {
  type ModuleIdent = id.T

  case object modpath {
    sealed trait T
    sealed case class MPfile(dirpath:kernelTypes.dirpath.T) extends T
    sealed case class MPbound(mbidT:mbid.T) extends T
    sealed case class MPdot(t:T,label:Label.T)
  }

  case object Label {
    type T
  }

  case object mbid {
    type T = (Int,kernelTypes.id.T,kernelTypes.dirpath.T)
  }
  case object id {
    type T = String
  }
  case object name {
    sealed trait T
    case object Anonymous extends name.T
    sealed case class Name(id:kernelTypes.id.T) extends name.T
  }
  case object globRef {
    type Variable = id.T
    sealed trait T
    //TODO: Determine if this level of detail is necessary for the AST (parameter are missing in ConstRef IndRef and ConstructRef
    sealed case class VarRef(v:Variable) extends T
    case object ConstRef extends T
    case object IndRef extends T
    case object ConstructRef extends T
  }

  case object sorts {
    sealed trait Family
    case object InProp extends Family
    case object InSet extends Family
    case object InType extends Family
  }

  case object constr {
    sealed trait CaseStyle
    case object LetStyle extends CaseStyle
    case object IfStyle extends CaseStyle
    case object LetPatternStyle extends CaseStyle
    case object MatchStyle extends CaseStyle
    case object RegularStyle extends CaseStyle
  }

  case object dirpath {
    /**
      * Type of directory paths. Essentially a list of module identifiers.
      * order is reversed to improve sharing. E.g. A.B.C is ["C";"B";"A"]
      */
    type T = List[ModuleIdent]

    val empty : T = Nil

    val make : List[ModuleIdent] => T = ids => ids

    val isEmpty : T => Boolean = t => t.isEmpty
  }

  object univ {

  }

  object declarations {
    sealed trait RecursivityKind
    case object Finite extends RecursivityKind
    case object CoFinite extends RecursivityKind
    case object BiFinite extends RecursivityKind
  }

  object cPrimitives {
    sealed trait T
    case object Int63head0 extends T
    case object Int63tail0 extends T
    case object Int63add extends T
    case object Int63sub extends T
    case object Int63mul extends T
    case object Int63div extends T
    case object Int63mod extends T
    case object Int63lsr extends T
    case object Int63lsl extends T
    case object Int63land extends T
    case object Int63lor extends T
    case object Int63lxor extends T
    case object Int63addc extends T
    case object Int63subc extends T
    case object Int63addCarryC extends T
    case object Int63subCarryC extends T
    case object Int63mulc extends T
    case object Int63diveucl extends T
    case object Int63div21 extends T
    case object Int63addMulDiv extends T
    case object Int63eq extends T
    case object Int63lt extends T
    case object Int63le extends T
    case object Int63compare extends T

    sealed trait PrimType
    case object PT_int63 extends PrimType

    sealed trait OpOrType
    sealed case class OtOp(t:T) extends OpOrType
    sealed case class OtType(primType: PrimType) extends  OpOrType
  }

  object convOracle {
    sealed trait _Level
    case object Expand extends _Level
    sealed case class Level(int: Int) extends _Level
    case object Opaque extends _Level

    lazy val default: _Level = Level(0)
    lazy val transparent: _Level = default
  }
}
