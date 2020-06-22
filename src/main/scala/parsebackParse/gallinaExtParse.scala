package parsebackParse

import parseback._
import implicits._
import ParserFix._
import model.interpTypes.constrExpr.constructor.ConstrExpr
import model.interpTypes.constrExpr.{AN, ByNotation, MApply, MIdent, MWith, ModuleAst, UniverseDeclExpr, WithDeclarationAst, WithDefinition, WithModule, constructor}
import model.interpTypes.impargs.{Implicit, ImplicitKind, MaximallyImplicit, NotImplicit}
import model.kernelTypes
import model.kernelTypes.{convOracle, name}
import model.kernelTypes.convOracle.{Expand, Level, Opaque}
import model.kernelTypes.id.T
import model.kernelTypes.name.{Anonymous, Name}
import model.libraryTypes.declKinds.{CanonicalStructure, Coercion, NoDischarge}
import model.libraryTypes.declareMods
import model.libraryTypes.declareMods.{Check, DefaultInline, Enforce, Inline, InlineAt, ModuleSignature, NoInline}
import model.pretypingTypes.globTerm.global
import model.pretypingTypes.typeclasses.HintInfoGen
import model.vernacTypes.vernacExpr
import model.vernacTypes.vernacExpr.{Ampersand, ArgumentSpec, Arguments, ArgumentsModifier, Assert, BeginSection, ClearBidiHint, ClearImplicits, ClearScopes, DefaultImplicits, EndSegment, ExtraScopes, FunClass, Id, Import, Include, ModuleAstInl, ModuleBinder, NameSectionHypSet, ReductionDontExposeCase, ReductionNeverUnfold, RefClass, Rename, Require, SectionSubsetExpr, Slash, SortClass, SsCompl, SsEmpty, SsFwdClose, SsSingl, SsSubstr, SsType, SsUnion}

object gallinaExtParse {

  implicit val W : parseback.Whitespace = parseback.Whitespace("""\s+""".r.?)

  lazy val gallina_ext = (
    ("Module":Parser[String]) ~?> opt_export_token ~ primParse.identref ~?
      module_binder.++ ~? opt_of_module_type ~? opt_is_module_expr ^^ {
      (_,eExp_tk,id,oModuleBinders,oModType,moduleAstInl) =>
        vernacExpr.DefineModule(eExp_tk.flatten,id,oModuleBinders.getOrElse(Nil),
          oModType.getOrElse(Check(Nil)),moduleAstInl.getOrElse(Nil))
    }
    | "Module" ~> "Type" ~> primParse.identref ~? module_binder.++ ~? opt_check_module_types ~? opt_is_module_type ^^ {
      (_,id,mBs,mTys,moduleAstInl) =>
        vernacExpr.DeclareModuleType(id,mBs.getOrElse(Nil),mTys.getOrElse(Nil),moduleAstInl.getOrElse(Nil))
    }
    | "Declare" ~> "Module" ~?> opt_export_token ~ helper.declare_mod_second_part ^^ {
      (_,optExp,dclMd2Part) => vernacExpr.DeclareModule(optExp.flatten,dclMd2Part.id,dclMd2Part.l,dclMd2Part.inlast)
    }
    | "Section" ~> primParse.identref ^^ BeginSection
    | "Chapter" ~> primParse.identref ^^ BeginSection
    | "End" ~> primParse.identref ^^ EndSegment
    | "Collection" ~> primParse.identref ~ (":=" ~> opt_section_subset_expr) ^^ NameSectionHypSet
    | ("Require":Parser[String]) ~?> opt_export_token ~ termParse.global.++ ^^ {(_,exp_tk,qidl) => Require(None,exp_tk.flatten,qidl)}
    | "From" ~> termParse.global ~ (("Require":Parser[String]) ~?> opt_export_token) ~ termParse.global.++ ^^ {
      (_,q,exp_tk,qidl) => Require(Some(q),exp_tk.flatten,qidl)
    }
    | "Import" ~> termParse.global.++ ^^ {(_,qidl) => Import(false,qidl)}
    | "Export" ~> termParse.global.++ ^^ {(_,qidl) => Import(true,qidl)}
    | "Include" ~> module_type_inl ~? ext_module_expr.++ ^^ {(_,mI,mA,oExprs) => Include((mI,mA) :: oExprs.getOrElse(Nil))}
    | "Include" ~> "Type" ~> module_type_inl ~? ext_module_type.++ ^^ {(_,mI,mA,oTypes) => Include((mI,mA) :: oTypes.getOrElse(Nil))}
    | "Transparent" ~> primParse.smart_global.++ ^^ {(_,l) => vernacExpr.SetOpacity(kernelTypes.convOracle.transparent, l)}
    | "Opaque" ~> primParse.smart_global.++ ^^ {(_,l) => vernacExpr.SetOpacity(kernelTypes.convOracle.Opaque, l)}
    | "Strategy" ~> (strategy_level ~ ("[" ~> primParse.smart_global.++ <~ "]")).++ ^^ vernacExpr.SetStrategy
    | ("Canonical":Parser[String]) ~?> "Structure" ~> termParse.global ~?
      (gallinaParse.univ_decl ?~ gallinaParse.def_body) ^^ {
      (_,q,ud) => ud match {
        case None => vernacExpr.Canonical(AN(q))
        case Some((u,d)) => val s = model.interpTypes.constrExprOps.coerceReferenceToId(q)
          vernacExpr.Definition((NoDischarge,CanonicalStructure),(Name(s),u),d)
      }
    }
      | ("Canonical":Parser[String]) ~?> "Structure" ~> primParse.by_notation ^^ {
      (_,s,oId) => vernacExpr.Canonical(ByNotation(s,oId))
    }
    | "Coercion" ~> termParse.global ~? gallinaParse.univ_decl ~ gallinaParse.def_body ^^ {
      (_,q,u,d) => val s = model.interpTypes.constrExprOps.coerceReferenceToId(q)
        vernacExpr.Definition((NoDischarge,Coercion),(Name(s),u),d)
    }
    | "Identity" ~> "Coercion" ~> primParse.identref ~ (":" ~> class_rawexpr) ~ (">->" ~> class_rawexpr) ^^ vernacExpr.IdentityCoercion
    | "Coercion" ~> termParse.global ~ (":" ~> class_rawexpr) ~ (">->" ~> class_rawexpr) ^^ {
      (_,q,c1,c2) => vernacExpr.Coercion(AN(q),c1,c2)
    }
    | "Coercion" ~> primParse.by_notation ~ (":" ~> class_rawexpr) ~ (">->" ~> class_rawexpr) ^^ {
      (_,s,os,c1,c2) => vernacExpr.Coercion(ByNotation(s,os),c1,c2)
    }
    | "Context" ~> termParse.binder.++ ^^ vernacExpr.Context
    | ("Instance":Parser[String]) ~?> opt_instance_name ~ (":" ~> termParse.oprConstr(200)) ~? opt_hint_info ~ helper.opt_instance_second_part ^^ {
      (_,namesup,t,info,props) => vernacExpr.Instance(
        namesup.getOrElse(((Anonymous,None),Nil))._1,
        namesup.getOrElse(((Anonymous,None),Nil))._2,
        t,
        props,
        info.getOrElse(HintInfoGen[ConstrExpr](None,None))
      )
    }
    | "Existing" ~> "Instances" ~> termParse.global.++ ~? ("|" ~> primParse.natural) ^^ {
      (_,qs,oI) => val info = HintInfoGen(oI,None)
        val insts = qs.map{(i) =>(i,info)}
        vernacExpr.ExistingInstance(insts)
    }
    | "Existing" ~> "Instance" ~> termParse.global ~? opt_hint_info ^^ {
      (_,q,info) => vernacExpr.ExistingInstance(List((q,info.getOrElse(HintInfoGen[ConstrExpr](None,None)))))
    }
    | "Existing" ~> "Class" ~> termParse.global ^^ vernacExpr.ExistingClass
    | "Arguments" ~> primParse.smart_global ~? argument_spec_block ~? (
      "," ~> ParserHelper.list1WithSperatorAndOptParser(more_implicits_block.++ ^^ {(_,l) => l.flatten},",")
    ) ~? (":" ~> ParserHelper.list1WithSeperator(arguments_modifier,",")) ^^ {
      (_, qid, args, more_implicits, mods) =>
        val newMods = mods match {
          case None => Nil
          case Some(m) => m.flatten
        }
        var slashPos: Option[Int] = None
        var ampersand_position: Option[Int] = None

        def parse_args(i: Int)(l: List[ArgumentSpec]): List[ArgumentSpec] = l match {
          case Nil => Nil
          case (x: Id) :: args => x :: parse_args(i + 1)(args)
          case Slash :: args => if (slashPos.isEmpty) {
            slashPos = Some(i);
            parse_args(i)(args)
          }
          else {
            throw new IllegalStateException("Slash modifier is only allowed once")
          }
          case Ampersand :: args => if (ampersand_position.isEmpty) {
            ampersand_position = Some(i);
            parse_args(i)(args)
          }
          else {
            throw new IllegalStateException("Ampersand modifier can only occur once")
          }
        }

        val newArgs = parse_args(0)(args.getOrElse(Nil))
        val newMore_implicits = more_implicits.getOrElse(Nil)
        vernacExpr.Arguments(qid, newArgs, newMore_implicits, slashPos, ampersand_position, newMods)
      }
      | "Implicit" ~> "Type" ~> reserv_list ^^ vernacExpr.Reserve
      | "Implicit" ~> "Types" ~> reserv_list ^^ vernacExpr.Reserve
      | "Generalizable" ~> ("All" ~> "Variables" ^^ Some(Nil) |
      "No" ~> "Variables" ^^ None |
      ("Variable"|"Variables") ~> primParse.identref ^^ Some) ^^ vernacExpr.Generalizable
    )

  object helper {

    sealed case class DeclareModuleSecondPart(id: kernelTypes.id.T, l: List[ModuleBinder], inlast: ModuleAstInl)

    lazy val declare_mod_second_part: Parser[DeclareModuleSecondPart] = primParse.identref ~? module_binder.++ ~ module_type_inl ^^ {
      (_, id, modBs, mA, mI) => DeclareModuleSecondPart(id, modBs.getOrElse(Nil), (mA, mI))
    }

    /**
      * optional with outcome None
      */
    lazy val opt_instance_second_part: Parser[Option[(Boolean, constructor.ConstrExpr)]] = (
      ":=" ~> "{" ~> termParse.record_declaration <~ "}" ^^ {(_,r) => Some((true,r))}
      | ":=" ~> termParse.lconstr ^^ ((_,c) => Some(false,c))
      )

    def starredidentreflist_to_expr(l:List[SectionSubsetExpr]):SectionSubsetExpr = l match {
      case Nil => SsEmpty
      case x :: xs => xs.foldRight(x){
        (i,acc) => SsUnion(i,acc)
      }
    }
  }

  /**
    * optional with outcome None
    */
  lazy val opt_export_token: Parser[Option[Boolean]] = (
    "Import" ^^ Some(false)
    | "Export" ^^ Some(true)
  )

  lazy val check_module_type: Parser[ModuleAstInl] = "<:" ~> module_type_inl
  /**
    * optional with outcome Nil
    */
  lazy val opt_check_module_types: Parser[List[ModuleAstInl]] = check_module_type.++

  /**
    * optional with outcome Check(Nil)
    */
  lazy val opt_of_module_type: Parser[ModuleSignature[ModuleAstInl]] = (
    ":" ~> module_type_inl ^^ {(_,mA,in) => Enforce((mA,in))}
    | opt_check_module_types ^^ { (_, mtys) => Check(mtys)}
    )

  lazy val module_type_inl: Parser[ModuleAstInl] = (
    "!" ~> module_type ^^ {(_,m) => (m,NoInline)}
      | module_type ~? opt_functor_app_annot ^^ {(_,m,a) => (m,a.getOrElse(DefaultInline))}
    )

  /**
    * optional with outcome Nil
    */
  lazy val opt_is_module_expr: Parser[List[ModuleAstInl]] =
    ":=" ~> module_expr_inl ~? ext_module_expr.++ ^^ { (_, mA, iL, oL) => (mA,iL) :: oL.getOrElse(Nil)}

  /**
    * optional with outcome Nil
    */
  lazy val opt_is_module_type: Parser[List[(ModuleAst, Inline)]] =
    ":=" ~> module_type_inl ~? ext_module_type.++ ^^ { (_, mA, mI, types) => (mA,mI) :: types.getOrElse(Nil)}

  lazy val ext_module_expr: Parser[ModuleAstInl] = "<+" ~> module_expr_inl

  lazy val ext_module_type: Parser[ModuleAstInl] = "<+" ~> module_type_inl


  /**
    * optional with outcome DefaultInline
    */
  lazy val opt_functor_app_annot: Parser[declareMods.Inline] = (
    "[" ~> "inline" ~> "at" ~> "level" ~> primParse.natural <~ "]" ^^ InlineAt
    | "[" ~> "no" ~> "inline" ~> "]" ^^ NoInline
  )

  lazy val module_expr_inl: Parser[ModuleAstInl] = (
    "!" ~> module_expr ^^ {(_,m) => (m,NoInline)}
      | module_expr ~? opt_functor_app_annot ^^ {(_,m,a) => (m,a.getOrElse(DefaultInline))}
  )

  lazy val module_binder: Parser[ModuleBinder] = ("(":Parser[String]) ~?> opt_export_token ~
    primParse.identref.++ ~ (":" ~> module_type_inl <~ ")") ^^ {
    (_,exp_tk,ids,mA,mI) => (exp_tk.flatten,ids,(mA,mI))
  }

  lazy val module_expr : Parser[ModuleAst] = module_expr_atom | module_expr ~ module_expr_atom ^^ MApply

  lazy val module_expr_atom : Parser[ModuleAst] = primParse.qualid ^^ MIdent | "(" ~> module_expr <~ ")"

  lazy val with_declaration : Parser[WithDeclarationAst] = (
    "Definition" ~> primParse.fullyqualid ~? gallinaParse.univ_decl ~ (":=" ~> termParse.lconstr) ^^ WithDefinition
    | "Module" ~> primParse.fullyqualid ~ (":=" ~> primParse.qualid) ^^ WithModule
    )

  lazy val module_type: Parser[ModuleAst] = (
    primParse.qualid ^^ MIdent
    | "(" ~> module_type <~ ")"
    | module_type ~ module_expr_atom ^^ MApply
    | module_type ~ ("with" ~> with_declaration) ^^ MWith
  )

  /**
    * Proof using
    */

  /**
    * optional with outcome starredidentreflist_to_expr(Nil)
    */
  lazy val opt_section_subset_expr: Parser[SectionSubsetExpr] =  (
      starredidentref.++ ^^ {(_,stids) => helper.starredidentreflist_to_expr(stids) }
        | ssexpr_50
      )

  //TODO: how to imlement only_starredidentrefs? Or is this necessary?

  lazy val starredidentref: Parser[SectionSubsetExpr] = (
    primParse.identref ^^ SsSingl
    | primParse.identref <~ "*" ^^ {(_,i) => SsFwdClose(SsSingl(i))}
    | "Type" ^^ SsType
    | "Type" ~> "*" ^^ SsFwdClose(SsType)
  )

  lazy val ssexpr_50 : Parser[SectionSubsetExpr] = (
    ssexpr_35 ~ ("-" ~> ssexpr_50) ^^ SsSubstr
    | ssexpr_35 ~ ("+" ~> ssexpr_50) ^^ SsUnion
    | ssexpr_35
    )

  lazy val ssexpr_35 : Parser[SectionSubsetExpr] = "-" ~> ssexpr_35 ^^ SsCompl | ssexpr_0

  lazy val ssexpr_0  : Parser[SectionSubsetExpr] = (
    starredidentref
    | ("(":Parser[String]) ~?> starredidentref.++ <~ ")" ^^ {(_,sirefs) => helper.starredidentreflist_to_expr(sirefs.getOrElse(Nil))}
      | ("(":Parser[String]) ~?> starredidentref.++ <~ ")" <~ "*" ^^ {(_,sirefs) => SsFwdClose(helper.starredidentreflist_to_expr(sirefs.getOrElse(Nil)))}
    | "(" ~> ssexpr_50 <~ ")"
    | "(" ~> ssexpr_50 <~ ")" <~ "*"
    )

  /**
    * optional with outcome ((Anonymous,None),Nil)
    */
  lazy val opt_instance_name: Parser[((Name, Option[UniverseDeclExpr]), List[constructor.LocalBinderExpr])] =
    gallinaParse.ident_decl ~ termParse.binders ^^ {(_,name_1,name_2,bl) => ((Name(name_1),name_2),bl)}

  lazy val arguments_modifier: Parser[List[ArgumentsModifier]] = (
    "simpl" ~> "nomatch" ^^ List(ReductionDontExposeCase)
    | "simpl" ~> "never" ^^ List(ReductionNeverUnfold)
    | "default" ~> "implicits" ^^ DefaultImplicits :: Nil
    | "clear" ~> "implicits" ^^ ClearImplicits :: Nil
    | "clear" ~> "scopes" ^^ ClearScopes :: Nil
    |"clear" ~> "bidirectionality" ~> "hint" ^^ ClearBidiHint :: Nil
    |"rename" ^^ Rename :: Nil
    | "assert" ^^ Assert :: Nil
    | "extra" ~> "scopes" ^^ ExtraScopes :: Nil
    | "clear" ~> "scopes" ~> "and" ~> "implicits" ^^ ClearImplicits :: ClearScopes :: Nil
    | "clear" ~> "implicits" ~> "and" ~> "scopes" ^^ ClearImplicits :: ClearScopes :: Nil
    )

  lazy val scope: Parser[String] = "%" ~> primParse._ident

  lazy val argument_spec_block:Parser[List[ArgumentSpec]] = (
    argument_spec ^^ {
      (_,n) => List(Id(n._1,n._2,n._3,NotImplicit))
    }
    | "/" ^^ List(Slash)
    | "&" ^^ List(Ampersand)
    | "(" ~> argument_spec.++ ~ ((")":Parser[String]) ~?> scope) ^^ {
      (_,argSpecs,_) => argSpecs.map{
        argSpec => Id(argSpec._1,argSpec._2,argSpec._3,NotImplicit)
      }
    }
    | "[" ~> argument_spec.++ ~ (("]":Parser[String]) ~?> scope ) ^^ {
      (_,argSpecs,_) => argSpecs.map{
        argSpec => Id(argSpec._1,argSpec._2,argSpec._3,Implicit)
      }
    }
    | "{" ~> argument_spec.++ ~ (("}":Parser[String]) ~?> scope ) ^^ {
      (_,argSpecs,_) => argSpecs.map{
        argSpec => Id(argSpec._1,argSpec._2,argSpec._3,MaximallyImplicit)
      }
    }
  )

  lazy val more_implicits_block: Parser[List[(name.T, ImplicitKind)]] = (
    primParse.name ^^ {(_,n) => (n,NotImplicit) :: Nil}
    | "[" ~> primParse.name.++ <~ "]" ^^ {(_,items) => items.map(n => (n,Implicit))}
    | "{" ~> primParse.name.++ <~ "}" ^^ {(_,items) => items.map(n => (n,MaximallyImplicit))}
  )

  lazy val argument_spec: Parser[(name.T, Boolean, Option[String])] = ("!":Parser[String]) ?~ primParse.name ~? scope ^^ {
    (_,b,n,oS) => (n,b.isDefined,oS)
  }

  /**
    * optional with outcome HintInfoGen[ConstrExpr](None,None)
    */
  lazy val opt_hint_info: Parser[HintInfoGen[ConstrExpr]] =
    ("|":Parser[String]) ~?> primParse.natural ~? termParse.constr_pattern ^^ {
      (_,oI,oT) => HintInfoGen(oI,oT)
    }

  lazy val strategy_level: Parser[convOracle._Level] = (
    "expand" ^^ Expand
    | "opaque" ^^ Opaque
    | primParse.integer ^^ Level
    | "transparent" ^^ convOracle.transparent
  )

  lazy val class_rawexpr: Parser[vernacExpr.ClassRawExpr] = (
    "Funclass" ^^ FunClass
    | "Sortclass" ^^ SortClass
    | primParse.smart_global ^^ RefClass
  )

  lazy val reserv_list : Parser[List[(List[T],ConstrExpr)]] = (
    reserv_tuple.++
    | simple_reserv ^^ {(_,st,sc) => List((st,sc))}
  )

  lazy val simple_reserv: Parser[(List[T], ConstrExpr)] = primParse.identref.++ ~ (":" ~> termParse.lconstr)

  lazy val reserv_tuple: Parser[(List[T], ConstrExpr)] = "(" ~> simple_reserv <~ ")"



}
