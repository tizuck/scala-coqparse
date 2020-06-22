package model

import model.interpTypes.constrExpr.OrByNotation
import model.libraryTypes.libNames.FullPath
import model.libraryTypes.libobject.ObjectName
import model.libraryTypes.nametab.ObjectPrefix

object libraryTypes {
  object genArg {
    sealed trait RawGenericArgument
  }

  object libobject {
    type ObjectName = (FullPath,kernelTypes.name.T)
  }

  object nametab {
    sealed case class ObjectPrefix(obj_dir:kernelTypes.dirpath.T,obj_mp:kernelTypes.modpath.T)
  }

  object libNames {
    /**
      * In the original implementation Qualid is more than just an ID, but for the sake of parsing it can be simplified
      *
      * Originally it is defined as:
      * {{{
      * type full_path = {
      * dirpath : DirPath.t ;
      * basename : Id.t }
      *
      * type qualid_r = full_path
      * type qualid = qualid_r CAst.t}}}
      *
      * @see [[https://github.com/coq/coq/blob/649b611b1b0e76a599637266e89538c9f2e6776c/library/libnames.ml]]
      */
    type Qualid = FullPath

    val make_qualid : kernelTypes.dirpath.T => kernelTypes.id.T => FullPath = pa => id => make_path(pa)(id)
    val make_path : kernelTypes.dirpath.T => kernelTypes.id.T => FullPath = pa => id => FullPath(pa,id)
    val qualid_of_ident : kernelTypes.id.T => Qualid = id => make_qualid(kernelTypes.dirpath.empty)(id)
    val qualid_is_ident : Qualid => Boolean = q =>  kernelTypes.dirpath.isEmpty(q.dirPath)
    val qualid_basename : Qualid => kernelTypes.id.T = qid => qid.basename

    sealed case class FullPath(dirPath:kernelTypes.dirpath.T,basename:kernelTypes.id.T)
  }

  object declKinds {
    sealed trait TheoremKind
    case object Theorem extends TheoremKind
    case object Lemma extends TheoremKind
    case object Fact extends TheoremKind
    case object Remark extends TheoremKind
    case object Property extends TheoremKind
    case object Proposition extends TheoremKind
    case object Corollary extends TheoremKind

    sealed trait Discharge
    case object DoDischarge extends Discharge
    case object NoDischarge extends Discharge

    sealed trait AssumptionObjectKind
    case object Definitional extends AssumptionObjectKind
    case object Logical extends AssumptionObjectKind
    case object Conjectural extends AssumptionObjectKind

    sealed trait DefinitionObjectKind
    case object Definition extends DefinitionObjectKind
    case object Coercion extends DefinitionObjectKind
    case object SubClass extends DefinitionObjectKind
    case object CanonicalStructure extends DefinitionObjectKind
    case object Example extends DefinitionObjectKind
    case object Fixpoint extends DefinitionObjectKind
    case object CoFixpoint extends DefinitionObjectKind
    case object Scheme extends DefinitionObjectKind
    case object StructureComponent extends DefinitionObjectKind
    case object IdentityCoercion extends DefinitionObjectKind
    case object Instance extends DefinitionObjectKind
    case object Method extends DefinitionObjectKind
    case object Let extends DefinitionObjectKind

  }

  object declareMods {
    sealed trait Inline
    case object NoInline extends Inline
    case object DefaultInline extends Inline
    case class InlineAt(i:Int) extends Inline

    sealed trait ModuleSignature[A]
    sealed case class Enforce[A](a:A) extends ModuleSignature[A]
    sealed case class Check[A](as:List[A]) extends ModuleSignature[A]
  }

  object lib {
    type Node
    type LibraryEntry = (ObjectName,Node)
    type LibrarySegment = List[LibraryEntry]
    sealed case class LibState(compName:Option[kernelTypes.dirpath.T] = None,
                               lib_stk:LibrarySegment = Nil,
                               path_prefix:ObjectPrefix)
  }
}
