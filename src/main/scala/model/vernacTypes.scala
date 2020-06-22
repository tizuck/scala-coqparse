package model

import model.gramlibTypes.gramext.GAssoc
import model.interpTypes.constrExpr
import model.interpTypes.constrExpr.{IdentDecl, ModuleAst, NameDecl, OrByNotation, UniverseDeclExpr}
import model.interpTypes.constrExpr.constructor.{ConstrExpr, LocalBinderExpr, RecursionOrderExpr}
import model.interpTypes.impargs.ImplicitKind
import model.interpTypes.notationTerm.ConstrAsBinderKind
import model.kernelTypes.convOracle._Level
import model.kernelTypes.declarations.RecursivityKind
import model.kernelTypes.name
import model.kernelTypes.sorts.Family
import model.libTypes.flags.CompatVersion
import model.libraryTypes.declKinds.{AssumptionObjectKind, DefinitionObjectKind, Discharge, TheoremKind}
import model.libraryTypes.declareMods.{Inline, ModuleSignature}
import model.libraryTypes.libNames.Qualid
import model.parsingTypes.extend.{ProductionLevel, SimpleConstrProdEntryKey}
import model.pretypingTypes.globTerm.GlobConstraint
import model.tacticsTypes.genRedExpr.RawRedExpr
import model.tacticsTypes.hints.HintInfoExpr

object vernacTypes {

  object vernacExpr {
    type VerboseFlag = Boolean
    type RecFlag = Boolean
    type CoercionFlag = Boolean
    type InstanceFlag = Option[Boolean]
    type InductiveFlag = RecursivityKind
    type ExportFlag = Boolean
    type ScopeName = String
    type ProofExpr = (interpTypes.constrExpr.IdentDecl,(List[LocalBinderExpr],ConstrExpr))
    type ConstructorExpr = WithCoercion[(kernelTypes.id.T,ConstrExpr)]
    type DeclNotation = (String,ConstrExpr,Option[ScopeName])
    type InductiveExpr = (WithCoercion[IdentDecl],List[LocalBinderExpr],
      Option[ConstrExpr],InductiveKind,ConstructorListOrRecordDeclExpr)
    type FixpointExpr = FixExprGen[Option[RecursionOrderExpr]]
    type CoFixpointExpr = FixExprGen[Unit]
    type SortExpr = Family
    type SimpleBinder = (List[kernelTypes.id.T],ConstrExpr)

    type WithCoercion [T] = (CoercionFlag,T)

    type FixExprGen[A] = (kernelTypes.id.T,Option[UniverseDeclExpr],A,List[LocalBinderExpr],ConstrExpr,Option[ConstrExpr],List[DeclNotation])

    type OnlyParsingFlag = Option[CompatVersion]

    sealed trait ClassRawExpr
    case object FunClass extends ClassRawExpr
    case object SortClass extends ClassRawExpr
    sealed case class RefClass(notation:OrByNotation[Qualid]) extends ClassRawExpr

    sealed trait VernacCumulativeType
    case object Cumulative extends VernacCumulativeType
    case object NonCumulative extends VernacCumulativeType

    sealed trait InductiveKind
    case object Inductive extends InductiveKind
    case object CoInductive extends InductiveKind
    case object Variant extends InductiveKind
    case object Record extends InductiveKind
    case object Structure extends InductiveKind
    sealed case class Class(b:Boolean) extends InductiveKind

    sealed trait DefinitionExpr
    sealed case class ProveBody(localBinderExprs: List[LocalBinderExpr],
                                constrExpr: ConstrExpr) extends DefinitionExpr
    sealed case class DefineBody(localBinderExprs: List[LocalBinderExpr],
                                 rawRedExpr: Option[RawRedExpr],
                                 constrExpr: ConstrExpr,
                                 oConstrExpr: Option[ConstrExpr]) extends DefinitionExpr

    sealed case class RecordFieldAttr(rf_subclass:InstanceFlag,rf_priority:Option[Int],rf_notation:List[DeclNotation],rf_canonical:Boolean)

    sealed trait _Scheme
    sealed case class InductionScheme(boolean: Boolean, qn:OrByNotation[Qualid], sg:SortExpr) extends _Scheme
    sealed case class CaseScheme(boolean: Boolean,orByNotation: OrByNotation[Qualid],sortExpr: SortExpr) extends _Scheme
    sealed case class EqualityScheme(orByNotation: OrByNotation[Qualid]) extends _Scheme

    type ModuleAstInl = (ModuleAst,Inline)
    type ModuleBinder = (Option[Boolean],List[kernelTypes.id.T],ModuleAstInl)

    sealed trait VernacExpr

    sealed case class Load(v:VerboseFlag, s:String) extends  VernacExpr
    sealed case class StartTheoremProof(tk:TheoremKind, p:List[ProofExpr]) extends VernacExpr
    sealed case class Assumption(assTok:(Discharge,AssumptionObjectKind),
                                 il:Inline,
                                 wC:List[WithCoercion[(List[IdentDecl],ConstrExpr)]]) extends VernacExpr
    sealed case class Definition(defTok:(Discharge,DefinitionObjectKind),
                                 nameDecl: NameDecl,
                                 definitionExpr: DefinitionExpr) extends VernacExpr
    sealed case class Inductive(cum:Option[VernacCumulativeType],boolean: Boolean, indFlag:InductiveFlag,ls:List[(InductiveExpr,List[DeclNotation])]) extends VernacExpr
    sealed case class Fixpoint(dis:Discharge,fixs:List[FixpointExpr]) extends VernacExpr
    sealed case class CoFixpoint(dis:Discharge,cofixs:List[CoFixpointExpr]) extends VernacExpr
    sealed case class Scheme(schemes:List[(Option[kernelTypes.id.T],_Scheme)]) extends VernacExpr
    sealed case class CombinedScheme(id:kernelTypes.id.T,ids:List[kernelTypes.id.T]) extends VernacExpr
    sealed case class Register(qualid: Qualid,regKind:RegisterKind) extends VernacExpr
    sealed case class Primitive(id:kernelTypes.id.T,opOrT:kernelTypes.cPrimitives.OpOrType,optT:Option[ConstrExpr]) extends VernacExpr
    sealed case class Universe(ids:List[kernelTypes.id.T]) extends VernacExpr
    sealed case class Constraint(globConstraint: List[GlobConstraint]) extends VernacExpr
    sealed case class DeclareModule(oB:Option[Boolean],id:kernelTypes.id.T,
                                    moduleBinders:List[ModuleBinder],moduleAstInl: ModuleAstInl) extends VernacExpr
    sealed case class DefineModule(oB:Option[Boolean],id:kernelTypes.id.T,moduleBinders:List[ModuleBinder],
                                   moduleSig: ModuleSignature[ModuleAstInl],moduleAstInls:List[ModuleAstInl]) extends VernacExpr
    sealed case class DeclareModuleType(id:kernelTypes.id.T,moduleBinders:List[ModuleBinder],
                                        moduleAstInls1: List[ModuleAstInl],moduleAstInls2: List[ModuleAstInl]) extends VernacExpr
    sealed case class BeginSection(id:kernelTypes.id.T) extends VernacExpr
    sealed case class EndSegment(id:kernelTypes.id.T) extends VernacExpr
    sealed case class Require(oqualid:Option[Qualid],oExportFlag: Option[ExportFlag],qualids:List[Qualid]) extends VernacExpr
    sealed case class Import(exportFlag: ExportFlag,qualids:List[Qualid]) extends VernacExpr
    sealed case class Include(moduleAstInls: List[ModuleAstInl]) extends VernacExpr
    sealed case class NameSectionHypSet(id:kernelTypes.id.T,sectionSubsetExpr: SectionSubsetExpr) extends VernacExpr
    sealed case class SetOpacity(level:_Level,notations:List[constrExpr.OrByNotation[Qualid]]) extends VernacExpr
    sealed case class SetStrategy(levelNotations:List[(_Level,List[OrByNotation[Qualid]])]) extends VernacExpr
    sealed case class Canonical(notation:OrByNotation[Qualid]) extends VernacExpr
    sealed case class Coercion(orByNotation: OrByNotation[Qualid],c1:ClassRawExpr,c2:ClassRawExpr) extends VernacExpr
    sealed case class IdentityCoercion(id:kernelTypes.id.T,c1:ClassRawExpr,c2:ClassRawExpr) extends VernacExpr
    sealed case class Context(l:List[LocalBinderExpr]) extends VernacExpr
    sealed case class ExistingInstance(instances:List[(Qualid,HintInfoExpr)]) extends VernacExpr
    sealed case class ExistingClass(q:Qualid) extends VernacExpr
    sealed case class Arguments(orByNotation: OrByNotation[Qualid],
                                argumentStats:List[ArgumentSpec],
                                extraImplicits:List[List[(kernelTypes.name.T,ImplicitKind)]],
                                nTriggerReduction:Option[Int],
                                nBidirectionalTyping:Option[Int],
                                argumentModifiers:List[ArgumentsModifier]) extends VernacExpr
    sealed case class Reserve(binders:List[SimpleBinder]) extends VernacExpr
    sealed case class Generalizable(optionalIds:Option[List[kernelTypes.id.T]]) extends VernacExpr
    sealed case class SyntacticDefinition(id:kernelTypes.id.T,
                                                idsTermAssoc:(List[kernelTypes.id.T],ConstrExpr),
                                                onlyParsing:OnlyParsingFlag) extends VernacExpr
    sealed case class Notation(c:ConstrExpr,ssyntaxMods:(String,List[SyntaxModifier]),scope:Option[ScopeName]) extends VernacExpr
    sealed case class NotationAddFormat(s1:String,s2:String,s3:String) extends VernacExpr
    sealed case class DeclareCustomEntry(s:String) extends VernacExpr
    sealed case class SyntaxExtension(b:Boolean,ssyntaxMods:(String,List[SyntaxModifier])) extends VernacExpr

    sealed case class Instance(name:NameDecl,binders:List[LocalBinderExpr],ty:ConstrExpr,body:Option[(Boolean,ConstrExpr)],hintInfo:HintInfoExpr) extends VernacExpr

    trait ArgumentSpec
    sealed case class Id(_name:name.T,recarg_like:Boolean,notation_scope:Option[String],impl:ImplicitKind) extends ArgumentSpec
    case object Slash extends ArgumentSpec
    case object Ampersand extends ArgumentSpec

    trait ArgumentsModifier
    case object ReductionDontExposeCase extends ArgumentsModifier
    case object ReductionNeverUnfold extends ArgumentsModifier
    case object Rename extends ArgumentsModifier
    case object ExtraScopes extends ArgumentsModifier
    case object Assert extends ArgumentsModifier
    case object ClearImplicits extends ArgumentsModifier
    case object ClearScopes extends ArgumentsModifier
    case object ClearBidiHint extends ArgumentsModifier
    case object DefaultImplicits extends ArgumentsModifier

    sealed trait SectionSubsetExpr
    case object SsEmpty extends SectionSubsetExpr
    case object SsType extends SectionSubsetExpr
    sealed case class SsSingl(id:kernelTypes.id.T) extends SectionSubsetExpr
    sealed case class SsCompl(ssexpr:SectionSubsetExpr) extends SectionSubsetExpr
    sealed case class SsUnion(ssexpr1:SectionSubsetExpr,ssexpr2:SectionSubsetExpr) extends SectionSubsetExpr
    sealed case class SsSubstr(ssexpr1:SectionSubsetExpr,ssexpr2:SectionSubsetExpr) extends SectionSubsetExpr
    sealed case class SsFwdClose(ssexpr:SectionSubsetExpr) extends SectionSubsetExpr

    sealed trait LocalDeclExpr
    sealed case class AssumExpr(name:kernelTypes.name.T,constrExpr: ConstrExpr) extends LocalDeclExpr
    sealed case class DefExpr(name:kernelTypes.name.T,constrExpr: ConstrExpr,oConstrExpr:Option[ConstrExpr])

    sealed trait ConstructorListOrRecordDeclExpr
    sealed case class Constructors(constructorExprs:List[ConstructorExpr]) extends ConstructorListOrRecordDeclExpr
    sealed case class RecordDecl(oId:Option[kernelTypes.id.T],lDeclRecFAtrr:List[(LocalDeclExpr,RecordFieldAttr)]) extends ConstructorListOrRecordDeclExpr

    sealed trait RegisterKind
    case object RegisterInline extends RegisterKind
    sealed case class RegisterCoqLib(qualid: Qualid) extends RegisterKind

    sealed trait SyntaxModifier
    sealed case class SetItemLevel(ss:List[String],oConstrAsBinderKind: Option[ConstrAsBinderKind],oProductionLevel:Option[ProductionLevel]) extends SyntaxModifier
    sealed case class SetLevel(n:Int) extends SyntaxModifier
    sealed case class SetCustomEntry(s:String,oN:Option[Int]) extends SyntaxModifier
    sealed case class SetAssoc(gAssoc:GAssoc) extends SyntaxModifier
    sealed case class SetEntryType(s:String,entryKey:SimpleConstrProdEntryKey) extends SyntaxModifier
    case object SetOnlyParsing extends SyntaxModifier
    case object SetOnlyPrinting extends SyntaxModifier
    sealed case class SetCompatVersion(compatVersion: CompatVersion) extends SyntaxModifier
    sealed case class SetFormat(s1:String,s2:String) extends SyntaxModifier
  }

}
