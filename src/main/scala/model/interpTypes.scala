package model

import parsebackParse._
import engineTypes.namegen.IntroAnonymous
import kernelTypes.constr.CaseStyle
import kernelTypes.{id, name}
import libraryTypes.libNames.Qualid
import model.engineTypes.uState.GenUniverseDecl
import model.interpTypes.constrExpr.{NotationEntry, NotationEntryLevel}
import model.interpTypes.constrExpr.constructor._
import model.interpTypes.notationTerm.Interpretation
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import pretypingTypes.CastType
import pretypingTypes.globTerm.{ExistentialName, GlobConstraint, GlobLevel, GlobSort}

object interpTypes {

  object deprecation {
    sealed case class T(since:Option[String],note:Option[String])
  }

  object syntaxDef {
    //TODO:left out deprecation
    sealed case class Syndef(syndef_pattern:Interpretation, syndef_onlyparsing:Boolean)
  }

  object constrExpr {
    type ProjFlag = Option[Int]

    type RawNaturalNumber = Int
    type Sign = Boolean

    type CofixExpr = (id.T,List[LocalBinderExpr],ConstrExpr,ConstrExpr)
    type BranchExpr = (List[List[CasesPatternExpr]],ConstrExpr)
    type CaseExpr = (ConstrExpr,Option[kernelTypes.name.T],Option[CasesPatternExpr])

    type FixExpr = (id.T,Option[RecursionOrderExpr],List[LocalBinderExpr],ConstrExpr,ConstrExpr)
    type InstanceExpr = List[GlobLevel]

    type NotationKey = String
    type Notation = (NotationEntryLevel,NotationKey)
    type CasesPatternNotationSubstitution = (List[CasesPatternExpr],List[List[CasesPatternExpr]])
    type ConstrNotationSubstitution = (List[ConstrExpr],List[List[ConstrExpr]],List[CasesPatternExpr],List[List[LocalBinderExpr]])

    type UniverseDeclExpr = GenUniverseDecl [List[id.T],List[GlobConstraint]]
    type IdentDecl = (id.T,Option[UniverseDeclExpr])
    type NameDecl = (kernelTypes.name.T,Option[UniverseDeclExpr])

    type ConstrPatternExpr = ConstrExpr

    sealed trait NotationEntryLevel
    case object InConstrEntrySomeLevel extends NotationEntryLevel
    sealed case class InCustomEntryLevel(s:String,i:Int) extends NotationEntryLevel

    sealed trait OrByNotation[+T]
    sealed case class AN[T](t:T) extends OrByNotation[T]
    sealed case class ByNotation(s1:String,s2:Option[String]) extends OrByNotation[Nothing]

    sealed trait PrimToken
    sealed case class Numeral(r:RawNaturalNumber, s:Sign) extends PrimToken
    //Different naming to avoid scope conflicts with String
    sealed case class SString(s:String) extends PrimToken

    case object constructor {
      sealed trait ConstrExpr
      sealed trait NotationConstrExpr extends ConstrExpr

      /**
        * used to enable polymorphic return type of function mkFix
        */
      sealed trait FixType extends ConstrExpr

      sealed case class Ref(q:Qualid,
                            i:Option[interpTypes.constrExpr.InstanceExpr]) extends ConstrExpr

      sealed case class Fix(i:id.T,
                            fs:List[interpTypes.constrExpr.FixExpr]) extends FixType

      sealed case class CoFix(i:id.T,
                              cs:List[interpTypes.constrExpr.CofixExpr]) extends FixType

      sealed case class ProdN(bll:List[LocalBinderExpr],
                              c:ConstrExpr) extends ConstrExpr

      sealed case class LambdaN(bll:List[LocalBinderExpr],
                                c:ConstrExpr) extends ConstrExpr

      sealed case class LetIn(n:kernelTypes.name.T,
                              c1:ConstrExpr,
                              oC2:Option[ConstrExpr]
                              , c3:ConstrExpr) extends  ConstrExpr


      sealed case class AppExpl (tr:(ProjFlag,Qualid,Option[interpTypes.constrExpr.InstanceExpr]),
                                 cs:List[ConstrExpr]) extends ConstrExpr

      sealed case class App (tu1:(ProjFlag,ConstrExpr),
                             lTu2:List[(ConstrExpr,Option[Explication])]) extends ConstrExpr

      sealed case class Record(tus:List[(Qualid,ConstrExpr)]) extends ConstrExpr

      sealed case class Cases(cS:CaseStyle,
                              oC:Option[ConstrExpr],
                              cs:List[CaseExpr],
                              bs:List[BranchExpr]) extends ConstrExpr

      sealed case class LetTuple(ns:List[name.T],
                                 tu:(Option[name.T],Option[ConstrExpr]),
                                 c1:ConstrExpr,
                                 c2:ConstrExpr) extends ConstrExpr

      sealed case class If(c1:ConstrExpr,
                           tu:(Option[name.T],Option[ConstrExpr]),
                           c2:ConstrExpr,
                           c3:ConstrExpr) extends ConstrExpr

      sealed case class Hole(oET:Option[engineTypes.evarKinds.T],
                             iP:engineTypes.namegen.IntroPatternNamingExpr,
                             oRgA:Option[libraryTypes.genArg.RawGenericArgument]) extends ConstrExpr

      sealed case class PatVar(patVar:pretypingTypes.pattern.PatVar) extends ConstrExpr

      sealed case class Cast(c:ConstrExpr,cC:CastType[ConstrExpr]) extends ConstrExpr

      sealed case class Notation(notation:constrExpr.Notation,notationSub:ConstrNotationSubstitution) extends ConstrExpr

      sealed case class NotationEntry(constrs:List[ConstrExpr] = Nil,rewritingStrategy:Strategy = org.bitbucket.inkytonik.kiama.rewriting.Rewriter.id) extends ConstrExpr

      sealed case class ReservedNotation(constrExprs:List[ConstrExpr] = Nil) extends ConstrExpr

      sealed case class Generalization(b:BindingKind,oA:Option[AbstractionKind],c:ConstrExpr) extends ConstrExpr

      sealed case class Prim(p:PrimToken) extends ConstrExpr

      sealed case class Evar(e:ExistentialName,ltu:List[(id.T,ConstrExpr)]) extends ConstrExpr

      sealed case class Sort(g:GlobSort) extends ConstrExpr

      sealed case class Delimiters(s:String,c:ConstrExpr) extends ConstrExpr

      sealed trait RecursionOrderExpr
      sealed case class  StructRec(id:kernelTypes.id.T) extends RecursionOrderExpr
      sealed case class WfRec(id:kernelTypes.id.T,c:ConstrExpr) extends RecursionOrderExpr
      sealed case class MeasureRec(oId:Option[kernelTypes.id.T],c:ConstrExpr,oC:Option[ConstrExpr]) extends RecursionOrderExpr

      sealed trait CasesPatternExpr
      sealed case class PatCast(cP:CasesPatternExpr,c: ConstrExpr) extends CasesPatternExpr
      sealed case class PatAlias(cP:CasesPatternExpr,n:kernelTypes.name.T) extends CasesPatternExpr
      sealed case class PatCstr(q:Qualid,oCps:Option[List[CasesPatternExpr]],cps:List[CasesPatternExpr]) extends CasesPatternExpr
      sealed case class PatAtom(oQ:Option[Qualid]) extends CasesPatternExpr
      sealed case class PatOr(cPs:List[CasesPatternExpr]) extends CasesPatternExpr
      //TODO: Is this compatible with my implementation of Notation ? Here it is a tuple of notation and substitution of notation
      /**
        * Notation n applied with substituion nS applied to arguments cPs
        *
        * @param n Notation
        * @param nS Substitution of notation
        * @param cPs List of arguments notation is applied on
        */
      sealed case class PatNotation(n:constrExpr.Notation,nS:CasesPatternNotationSubstitution,cPs:List[CasesPatternExpr]) extends CasesPatternExpr
      sealed case class PatPrim(p:PrimToken) extends CasesPatternExpr
      sealed case class PatRecord(rps:List[(Qualid,CasesPatternExpr)]) extends CasesPatternExpr
      sealed case class PatDelimiters(s:String,cP:CasesPatternExpr) extends CasesPatternExpr

      /**
        * Base-type for binders
        */
      sealed trait LocalBinderExpr

      //TODO: Left to right Accent in scaladoc
      /**
        * Local Assumption generated by usage of binders
        * Mainly used by [[parsebackParse.termParse.closed_binder]] and
        * [[parsebackParse.termParse.open_binders]]
        *
        * @param names Names associated with the local Assumption
        * @param b Deciedes if there is a left to right accent before the binder
        * @param c Type of binded names
        */
      sealed case class LocalAssum(names:List[kernelTypes.name.T],
                                   b:BinderKind,
                                   c:ConstrExpr) extends LocalBinderExpr

      /**
        * Local definition within a binder.
        *
        * Mainly used by [[termParse.closed_binder]]
        *
        * @example {{{(a := Nil)
        *  (a : List Nat = Nil)}}}
        * @param n names associated to that local definition
        * @param c1 Type of names
        * @param oC2 Definition
        */
      sealed case class LocalDef(n:kernelTypes.name.T,
                                 c1:ConstrExpr,
                                 oC2:Option[ConstrExpr]) extends LocalBinderExpr

      /**
        * Binder consisting of a pattern which is parsed by rules of [[termParse.Pattern]]
        *
        * @param tu Containing the pattern and if the pattern is an instance of [[interpTypes.constrExpr.constructor.PatCast]]
        *           a type of PatCast
        */
      sealed case class LocalPattern(tu:(CasesPatternExpr,Option[ConstrExpr])) extends LocalBinderExpr
    }

    sealed trait Explication
    sealed case class ExplByPos(i:Int,oId:Option[id.T]) extends Explication
    sealed case class ExplByName(id:kernelTypes.id.T) extends Explication


    /**
      * Base-type for two existing binder kinds.
      */
    sealed trait BinderKind

    /**
      * Base-type for differentiating between types of parenthesis
      */
    sealed trait BindingKind

    /**
      * Indicates binder using curly-parenthesis
      */
    case object Implicit extends BindingKind

    /**
      * Indicates binder using regular parenthesis
      */
    case object Explicit extends BindingKind


    /**
      * Indicates Default Binder
      *
      * If a binder starts with a left to right accent then b will be of Type
      * Generalized, else Default
      *
      * @param b type of parenthesis of binder
      */
    sealed case class Default(b:BindingKind) extends BinderKind

    /**
      * Indicates Binder starting with a left to right accent
      *
      * @param b1      todo
      * @param b2      Indicates the type of parenthesis this binder uses
      * @param boolean Indicates that the rule [[termParse.typeclass_constraint]] parsed an expression containing a !-Symbol
      */
    sealed case class Generalized(b1:BindingKind,
                                  b2:BindingKind,
                                  boolean: Boolean) extends BinderKind

    sealed trait AbstractionKind
    case object AbsLambda extends AbstractionKind
    case object AbsPi extends AbstractionKind

    sealed trait WithDeclarationAst
    sealed case class WithModule(ids:List[id.T],q:Qualid) extends WithDeclarationAst
    sealed case class WithDefinition(ids:List[id.T],oUnivDecl:Option[UniverseDeclExpr],c:ConstrExpr) extends WithDeclarationAst

    sealed trait ModuleAst
    sealed case class MIdent(q:Qualid) extends ModuleAst
    sealed case class MApply(ma1:ModuleAst,ma2:ModuleAst) extends ModuleAst
    sealed case class MWith(ma:ModuleAst,wda:WithDeclarationAst) extends ModuleAst

    sealed trait NotationEntry
    case object InConstrEntry extends NotationEntry
    sealed case class InCustomEntry(s:String) extends NotationEntry
  }

  //TODO: Throw meaningful exceptions
  object constrExprOps {
    val mkAppPattern : CasesPatternExpr => List[CasesPatternExpr] => CasesPatternExpr = cp => cps => cp match {
      case PatAtom(Some(r)) => PatCstr(r,None, cps)
      case PatCstr(r,None,l2) => throw new IllegalStateException("Not Allowed")
      case PatCstr(r,l1,l2) => PatCstr(r,l1,l2 ++ cps)
      case PatNotation(n,s,l) => PatNotation(n,s,l++cps)
      case _ => throw new IllegalStateException("Not Allowed")
    }

    val coerceReferenceToId: Qualid => kernelTypes.id.T = qid => {
      if(libraryTypes.libNames.qualid_is_ident(qid)){
        libraryTypes.libNames.qualid_basename(qid)
      } else {
        throw new IllegalStateException("This expression should be a simple identifier")
      }
    }
  }

  object notationTerm {

    sealed trait NotationConstr
    sealed case class NRef(globRef:kernelTypes.globRef.T) extends NotationConstr
    sealed case class NVar(id:kernelTypes.id.T) extends NotationConstr
    sealed case class NApp(c:NotationConstr,cs:List[NotationConstr]) extends NotationConstr

    sealed trait NotationVarInternalizationType
    case object NtnInternTypeAny extends NotationVarInternalizationType
    case object NtnInternTypeOnlyBinder extends NotationVarInternalizationType

    sealed trait ConstrAsBinderKind
    case object AsIdent extends ConstrAsBinderKind
    case object AsIdentOrPattern extends ConstrAsBinderKind
    case object AsStrictPattern extends ConstrAsBinderKind

    sealed trait ConstrEntryKeyGen[+A]
    case object ETIdent extends ConstrEntryKeyGen[Nothing]
    case object ETGlobal extends ConstrEntryKeyGen[Nothing]
    case object ETBigint extends ConstrEntryKeyGen[Nothing]
    sealed case class ETBinder(isOpenBindersList:Boolean) extends ConstrEntryKeyGen[Nothing]
    sealed case class ETConstr[A](notationEntry:NotationEntry,oConstrAsBinderKind:Option[ConstrAsBinderKind],a:A) extends ConstrEntryKeyGen[A]
    sealed case class ETPattern(isStrictPattern:Boolean,n:Option[Int]) extends ConstrEntryKeyGen[Nothing]

    type ScopeName = String
    type TmpScopeName = ScopeName
    type SubScopes = (Option[TmpScopeName],List[ScopeName])
    type Interpretation = (kernelTypes.id.T,List[(ExtendedSubScopes,NotationVarInternalizationType)],NotationConstr)
    type ExtendedSubScopes = (NotationEntryLevel,SubScopes)
  }

  object notationOps {
    val ldotsVar = ".."
  }

  object impargs {
    sealed trait ImplicitKind
    case object Implicit extends ImplicitKind
    case object MaximallyImplicit extends ImplicitKind
    case object NotImplicit extends ImplicitKind
  }
  object HVals {
    import interpTypes.constrExpr._

    val mkProdCN : List[LocalBinderExpr] => ConstrExpr => ConstrExpr = bll => c => {
      bll match {
        case Nil => c
        case _ => ProdN(bll,c)
      }
    }

    val mkLambdaCN : List[LocalBinderExpr] => ConstrExpr => ConstrExpr = bll => c => {
      bll match {
        case Nil => c
        case _ => LambdaN(bll,c)
      }
    }

    val mkSingleFix : Boolean => ((id.T, List[LocalBinderExpr], Option[RecursionOrderExpr], ConstrExpr, Option[ConstrExpr])) => FixType = kw => dcl => {
      val id = dcl._1
      mkFix(kw)(id)(List(dcl))
    }

    val mkFix : Boolean => id.T => List[(id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr])] => FixType = kw => id => dcls => {
      if(kw){
        val fb = dcls.map(dcl => mkFixB(dcl))
        Fix(id,fb)
      }
      else {
        val fb = dcls.map(dcl => mkCoFixB(dcl))
        CoFix(id,fb)
      }
    }

    val mkFixB : Tuple5[id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr]] => FixExpr = dcl => {
      val ty = dcl._5 match { case Some(ty) => ty case None => Hole(None,IntroAnonymous,None)}
      (dcl._1,dcl._3,dcl._2,ty,dcl._4)
    }

    //TODO: Find out what exactly the following line of code means and what it tries to avoid (Assumption: No Annotation is allowed, but how is this realized?)
    //let _ = Option.map (fun { CAst.loc = aloc } ->
    //    CErrors.user_err ?loc:aloc
    //      ~hdr:"Constr:mk_cofixb"
    //      (Pp.str"Annotation forbidden in cofix expression.")) (fst ann) in
    val mkCoFixB : Tuple5[id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr]] => CofixExpr = dcl => {
      val ty = dcl._5 match {case Some(t) => t case None => Hole(None,IntroAnonymous,None)}
      (dcl._1,dcl._2,ty,dcl._4)
    }

    val aliasvar : CasesPatternExpr => Option[name.T] = {
      case PatAlias(_, n) => Some(n)
      case _ => None
    }
  }
}
