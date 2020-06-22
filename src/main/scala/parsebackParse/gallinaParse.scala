package parsebackParse

import model.libraryTypes.declKinds._
import model.vernacTypes.vernacExpr._
import parseback._
import ParserFix._
import ParserHelper._
import implicits._
import model.engineTypes.namegen.IntroAnonymous
import model.engineTypes.uState.GenUniverseDecl
import model.interpTypes.constrExpr
import model.interpTypes.constrExpr.{IdentDecl, NameDecl, UniverseDeclExpr}
import model.interpTypes.constrExpr.constructor.{Cast, ConstrExpr, Hole, LocalBinderExpr, LocalPattern, RecursionOrderExpr}
import model.{interpTypes, kernelTypes, vernacTypes}
import model.kernelTypes.{cPrimitives, declarations}
import model.kernelTypes.declarations.{BiFinite, CoFinite, Finite}
import model.kernelTypes.id.T
import model.kernelTypes.name.Name
import model.libTypes.acyclicGraph.{Eq, Le, Lt}
import model.libraryTypes.declareMods.{DefaultInline, Inline, InlineAt, NoInline}
import model.libraryTypes.libNames.Qualid
import model.pretypingTypes.CastConv
import model.pretypingTypes.globTerm.GlobConstraint
import model.tacticsTypes.genRedExpr
import model.tacticsTypes.genRedExpr.RawRedExpr
import model.vernacTypes.vernacExpr

//TODO: https://github.com/coq/coq/blob/737955a82676cab8de7283bf23db3962dd6a3792/parsing/pcoq.mli type with these types

object gallinaParse {
  implicit val W:parseback.Whitespace = Whitespace("""\s+""".r.?)

  object functions {
    def test_plural_form(assums:List[(Boolean,(List[IdentDecl],ConstrExpr))]):Unit = {
      if (assums.size == 1) throw new IllegalStateException("Multiple assumptions are expected")
    }
  }

  lazy val gallina : Parser[VernacExpr] = (
    thm_token ~ ident_decl ~? termParse.binders ~ ":" ~ termParse.lconstr ~? helper.gallina.theorem_helper.++ ^^ {
      (_:List[Line], tk:(TheoremKind,(model.kernelTypes.id.T,Option[UniverseDeclExpr])),
       bs:Option[List[LocalBinderExpr]], _:String, c:ConstrExpr, withC:Option[List[ProofExpr]])
      => {
        StartTheoremProof(tk._1,(tk._2,(bs.getOrElse(Nil),c))::withC.getOrElse(Nil))
      }
    }
    | assumption_token ~? inline ~ assum_list ^^ {
      (_:List[Line],d:Discharge,aok:AssumptionObjectKind,
        il:Option[Inline],assums:List[(Boolean,(List[IdentDecl],ConstrExpr))]) =>
      Assumption((d,aok),il.getOrElse(NoInline),assums)
    }
    | assumptions_token ~? inline ~ assum_list ^^ {
      (_:List[Line],s:String,d:Discharge,aok:AssumptionObjectKind,in:Option[Inline],
        assums:List[(Boolean,(List[IdentDecl],ConstrExpr))]) =>
        //Original parser warns if List of Ident Decls only contains one Element
        //More formally : Original parser places the assumption wrapped in a Warning into the AST
        //TODO throw exception if above is the case ?
        functions.test_plural_form(assums)
        Assumption((d,aok),in.getOrElse(NoInline),assums)
    }
    | def_token ~ ident_decl ~ def_body ^^ {
      (_,dis,dok,id,udl,defe) =>
        model.vernacTypes.vernacExpr.Definition((dis,dok),helper.gallina.name_of_ident_decl((id,udl)),defe)
    }
    | "Let" ~ primParse.identref ~ def_body ^^ {(_,_,id,defe) =>
      model.vernacTypes.vernacExpr.Definition((DoDischarge,Let), (helper.gallina.name_of_ident(id),None),defe)
    }
    | opt2(cumulative_token, opt_private_token) ?~ finite_token ~ list1WithSeperator(inductive_definition,"with") ^^ {
      (_, cumPriv,fInd,fRec,indl) => {
        val indl_new = indl.map {
          case ((a, b, c, d), e) => ((a,b,c,fInd,d),e)
        }
        cumPriv match {
          case Some(cP) => Inductive(cP._1,cP._2.getOrElse(false),fRec,indl_new)
          case None => Inductive(None,false,fRec,indl_new)
        }
      }
    }
    | "Fixpoint" ~> list1WithSeperator(rec_definition,"with") ^^ {
      (_,recDefs) => vernacTypes.vernacExpr.Fixpoint(NoDischarge,recDefs)
    }
    | "Let" ~> "Fixpoint" ~> list1WithSeperator(rec_definition,"with") ^^ {
      (_,recDefs) => vernacExpr.Fixpoint(DoDischarge,recDefs)
    }
    |"CoFixpoint" ~> list1WithSeperator(corec_definition,"with") ^^ {
      (_,corecDefs) =>vernacExpr.CoFixpoint(NoDischarge,corecDefs)
    }
    | "Let" ~> "CoFixpoint" ~> list1WithSeperator(corec_definition,"with") ^^ {
      (_,corecDefs) =>vernacExpr.CoFixpoint(DoDischarge,corecDefs)
    }
    | "Scheme" ~> list1WithSeperator(scheme,"with") ^^ vernacExpr.Scheme
    | "Combined" ~> "Scheme" ~> primParse.identref ~ ("from" ~> list1WithSeperator(primParse.identref,",")) ^^ vernacExpr.CombinedScheme
    | "Register" ~> termParse.global ~ ("as" ~> primParse.qualid) ^^ {
      (_,q1,q2) => vernacExpr.Register(q1,RegisterCoqLib(q2))
    }
    | "Register" ~> "Inline" ~> termParse.global^^ {
      (_,g) => vernacExpr.Register(g,RegisterInline)
    }
    | "Primitive" ~> primParse.identref ~? (":" ~> termParse.lconstr) ~
      (":=" ~> register_token)  ^^ {(_,id,typopt,r) => vernacExpr.Primitive(id,r,typopt)}
    | ("Universe"|"Universes") ~> primParse.identref.++ ^^ vernacExpr.Universe
    | "Constraint" ~> list1WithSeperator(univ_constraint,",") ^^ vernacExpr.Constraint
    )

  lazy val register_token: Parser[cPrimitives.OpOrType] = (
    register_prim_token ^^ kernelTypes.cPrimitives.OtOp
    | register_type_token ^^ kernelTypes.cPrimitives.OtType
    )

  lazy val register_type_token: Parser[cPrimitives.PrimType] = "#int63_type" ^^ { (_, s) => kernelTypes.cPrimitives.PT_int63}

  lazy val register_prim_token: Parser[kernelTypes.cPrimitives.T] = (
    "#int63_head0" ^^ kernelTypes.cPrimitives.Int63head0
    | "#int63_tail0" ^^ kernelTypes.cPrimitives.Int63tail0
    | "#int63_add" ^^ kernelTypes.cPrimitives.Int63add
      | "#int63_sub" ^^ kernelTypes.cPrimitives.Int63sub
      | "#int63_mul" ^^ kernelTypes.cPrimitives.Int63mul
      | "#int63_div" ^^ kernelTypes.cPrimitives.Int63div
      | "#int63_mod" ^^ kernelTypes.cPrimitives.Int63mod
      | "#int63_lsr" ^^ kernelTypes.cPrimitives.Int63lsr
      | "#int63_lsl" ^^ kernelTypes.cPrimitives.Int63lsl
      | "#int63_land" ^^ kernelTypes.cPrimitives.Int63land
      | "#int63_lor" ^^ kernelTypes.cPrimitives.Int63lor
      | "#int63_lxor" ^^ kernelTypes.cPrimitives.Int63lxor
      | "#int63_addc" ^^ kernelTypes.cPrimitives.Int63addc
      | "#int63_subc" ^^ kernelTypes.cPrimitives.Int63subc
      | "#int63_addcarryc" ^^ kernelTypes.cPrimitives.Int63addCarryC
      | "#int63_subcarryc" ^^ kernelTypes.cPrimitives.Int63subCarryC
      | "#int63_mulc" ^^ kernelTypes.cPrimitives.Int63mulc
      | "#int63_diveucl" ^^ kernelTypes.cPrimitives.Int63diveucl
      | "#int63_div21" ^^ kernelTypes.cPrimitives.Int63div21
      | "#int63_addmuldiv" ^^ kernelTypes.cPrimitives.Int63addMulDiv
      | "#int63_eq" ^^ kernelTypes.cPrimitives.Int63eq
      | "#int63_lt" ^^ kernelTypes.cPrimitives.Int63lt
      | "#int63_le" ^^ kernelTypes.cPrimitives.Int63le
      | "#int63_compare" ^^ kernelTypes.cPrimitives.Int63compare
  )

  lazy val cumulative_token: Parser[vernacExpr.VernacCumulativeType] = (
    "Cumulative" ^^ {(_,_) => Cumulative}
    | "NonCumulative" ^^ {(_,_) => NonCumulative}
    )

  //TODO: potentially epsilon with false as result
  lazy val opt_private_token: Parser[Boolean] = "Private" ^^ { (_, _) => true}

  lazy val finite_token: Parser[(InductiveKind, declarations.RecursivityKind)] = (
    "Inductive" ^^ {(_,_) => (Inductive,Finite)}
    | "CoInductive" ^^ {(_,_) => (CoInductive,CoFinite)}
    | "Variant" ^^ {(_,_) => (Variant,BiFinite)}
    | "Record" ^^ {(_,_) => (Record,BiFinite)}
    | "Structure" ^^ {(_,_) =>(Structure,BiFinite) }
    | "Class" ^^ {(_,_) => (Class(true),BiFinite)}
  )

  lazy val thm_token : Parser[TheoremKind] = (
    "Theorem" ^^ {(_,_) => Theorem}
    | "Lemma" ^^ {(_,_) => Lemma}
    | "Fact" ^^ {(_,_) => Fact}
    | "Remark" ^^ {(_,_) => Remark}
    | "Property" ^^ {(_,_) => Property}
    | "Proposition" ^^ {(_,_) => Proposition}
    | "Corollary" ^^ {(_,_) => Corollary}
  )

  lazy val assumption_token : Parser[(Discharge,AssumptionObjectKind)] = (
    "Hypothesis" ^^ {(_,_) => (DoDischarge,Logical)}
    | "Variable" ^^ {(_,_) => (DoDischarge,Definitional)}
    | "Axiom" ^^ {(_,_) => (NoDischarge,Logical)}
    | "Parameter" ^^ {(_,_) => (NoDischarge,Definitional)}
    | "Conjecture" ^^ {(_,_) => (NoDischarge,Conjectural)}
  )

  lazy val assumptions_token : Parser[(String,(Discharge,AssumptionObjectKind))] = (
    "Hypotheses" ^^ {(_,_) => ("Hypotheses",(DoDischarge,Logical))}
    | "Variables" ^^ {(_,_) => ("Variables",(DoDischarge,Definitional))}
    | "Axioms" ^^ {(_,_) => ("Axioms",(NoDischarge,Logical))}
    | "Parameters" ^^ {(_,_) => ("Parameters",(NoDischarge,Definitional))}
    | "Conjectures" ^^ {(_,_) => ("Conjectures",(NoDischarge,Conjectural))}
  )

  lazy val def_token : Parser[(Discharge,DefinitionObjectKind)] = (
    "Definition" ^^ {(_,_) => (NoDischarge,model.libraryTypes.declKinds.Definition)}
    | "Example" ^^ {(_,_) => (NoDischarge,Example)}
    | "SubClass" ^^ {(_,_) => (NoDischarge,SubClass)}
  )

  lazy val one_decl_notation: Parser[(String, ConstrExpr, Option[kernelTypes.id.T])] = primParse.ne_string ~
    (":=" ~> termParse.constr ~? (":" ~> primParse.ident ^^ { (_, id) => id})) ^^ {
    (_,ntn,constr,oId) => (ntn,constr,oId)
  }
  //TODO: potentially epsilon with Nil as result
  lazy val opt_decl_notation: Parser[List[(String, ConstrExpr, Option[kernelTypes.id.T])]] = "where" ~> list1WithSeperator(one_decl_notation,"and") ^^ { (_, l) => l}

  //TODO: potentially epsilon with RecordDecl(None,Nil) as result
  lazy val opt_constructors_or_fields =
    ":=" ~> constructor_list_or_record_decl ^^ {(_,lc) => lc}


  lazy val inductive_definition =
    opt_coercion ?~ ident_decl ~ termParse.binders ~?
      opt3(":" ~> termParse.lconstr ^^ {(_,c) => c},opt_constructors_or_fields,opt_decl_notation) ^^ {
      (_:List[Line],
        oc:Option[Boolean],
        idI:kernelTypes.id.T,
        idU:Option[UniverseDeclExpr],
        indpar:List[LocalBinderExpr],
        oClCNtn:Option[(Option[ConstrExpr],
          Option[Constructors],
          Option[List[(String,ConstrExpr,Option[kernelTypes.id.T])]])]) =>
        oClCNtn match {
          case Some(clcntn) => (((oc.getOrElse(false),(idI,idU)),indpar,clcntn._1,clcntn._2.getOrElse(RecordDecl(None,Nil))),clcntn._3.getOrElse(Nil))
          case None => (((oc.getOrElse(false),(idI,idU)),indpar,None,RecordDecl(None,Nil)),Nil)
        }
    }

  lazy val constructor_list_or_record_decl = (
    "|" ~> list1WithSeperator(constructor,"|") ^^ {(_,l) => Constructors(l)}
  )


  //TODO: potentially epsilon with false as result
  lazy val opt_coercion: Parser[Boolean] = ">" ^^ { (_, _) => true}

  lazy val rec_definition: Parser[
    (kernelTypes.id.T,
      Option[UniverseDeclExpr],
      Option[RecursionOrderExpr],
      List[LocalBinderExpr],
      ConstrExpr,
      Option[ConstrExpr],
      List[(String, ConstrExpr, Option[T])])
  ] = ident_decl ~ termParse.opt_binders_fixannot ~? opt_type_cstr ~?
    (":=" ~> termParse.lconstr ^^ {(_,defi) => defi}) ~? opt_decl_notation ^^ {
     (_,identDecl,bl,rtype,body_def,notations) => {
      (
        identDecl._1,
        identDecl._2,
        bl._2,
        bl._1,
        rtype.getOrElse(Hole(None,IntroAnonymous,None)),
        body_def,
        notations.getOrElse(Nil)
      )
    }
  }

  lazy val corec_definition = ident_decl ~ termParse.binders ~? opt_type_cstr ~?
    (":=" ~> termParse.lconstr) ~? opt_decl_notation ^^ {
    (_, identDecl, binders, rtype, body_def, notations) =>
      (
        identDecl._1,
        identDecl._2,
        (),
        binders,
        rtype.getOrElse(Hole(None,IntroAnonymous,None)),
        body_def,
        notations.getOrElse(Nil)
      )
  }

  //TODO: potentially epsilon with Hole(None,IntroAnonymous,None) as result
  lazy val opt_type_cstr: Parser[ConstrExpr] = ":" ~> termParse.lconstr ^^ { (_, c) => c}

  lazy val scheme: Parser[(Option[T], _Scheme)] = (
    scheme_kind  ^^ { (_,kind) => (None,kind)}
    | primParse.identref ~ (":=" ~> scheme_kind) ^^ {(_,id,kind) => (Some(id),kind)}
  )

  lazy val scheme_kind: Parser[_Scheme] = (
    "Induction" ~> "for" ~> primParse.smart_global ~ ("Sort" ~> termParse.sort_family) ^^ {(_,ind,s) => InductionScheme(true,ind,s)}
    | "Minimality" ~> "for" ~> primParse.smart_global  ~ ("Sort" ~> termParse.sort_family) ^^{(_,ind,s) => InductionScheme(false,ind,s)}
    | "Elimination" ~> "for" ~> primParse.smart_global  ~ ("Sort" ~> termParse.sort_family) ^^{(_,ind,s) => CaseScheme(true,ind,s)}
    | "Case" ~> "for" ~> primParse.smart_global  ~ ("Sort" ~> termParse.sort_family) ^^{(_,ind,s) => CaseScheme(false,ind,s)}
    | "Equality" ~> "for" ~> primParse.smart_global ^^ {(_,ind) => EqualityScheme(ind)}
  )

  lazy val def_body: Parser[DefinitionExpr] = (
    parsebackParse.termParse.binders ?~ ":=" ~? reduce ~ parsebackParse.termParse.lconstr ^^ {
      (_,bs,_,red,c) =>
        if(bs.getOrElse(Nil).exists { case _:LocalPattern => true case _ => false }){
          val cNew = model.interpTypes.HVals.mkLambdaCN(bs.getOrElse(Nil))(c)
          DefineBody(bs.getOrElse(Nil),red.flatten,cNew,None)
        }
        else{
          c match {
            case Cast(cNew, CastConv(a)) => DefineBody(bs.getOrElse(Nil),red.flatten,cNew,Some(a))
            case _ => DefineBody(bs.getOrElse(Nil),red.flatten,c,None)
          }
        }
    }
    | parsebackParse.termParse.binders ?~ ":" ~ parsebackParse.termParse.lconstr ~ ":=" ~? reduce ~
      parsebackParse.termParse.lconstr ^^ {
      (_:List[Line],tu:(Option[List[LocalBinderExpr]],String),c1:ConstrExpr,
       _:String,rre:Option[Some[RawRedExpr]],c2:ConstrExpr) =>
         val ret = {
           if(tu._1.getOrElse(Nil).exists { case _:LocalPattern => true case _ => false }) {
             val c = Cast(c2, CastConv(c1))
             ((Nil, model.interpTypes.HVals.mkLambdaCN(tu._1.getOrElse(Nil))(c)), None)
           }
           else {
             ((tu._1.getOrElse(Nil),c2),Some(c1))
           }
         }
        DefineBody(ret._1._1,rre.flatten,ret._1._2,ret._2)
    }
    | parsebackParse.termParse.binders ?~ ":" ~ parsebackParse.termParse.lconstr ^^ {
      (_,bs,_,c) => ProveBody(bs.getOrElse(Nil),c)
    }
  )
  //TODO:Potentially Epsilon with None as result
  lazy val reduce: Parser[Some[genRedExpr.RedExprGen[ConstrExpr, constrExpr.OrByNotation[Qualid], ConstrExpr]]] = "Eval" ~ parsebackParse.ltac.tacticParse.red_expr ~ "in" ^^ {
    (_,_,r,_) => Some(r)
  }



  //TODO Potentially Epsilon with outcome NoInline
  lazy val inline : Parser[Inline] = (
    "Inline" ~ "(" ~ primParse.int ~ ")" ^^ {(_,_,_,i,_) => InlineAt(i)}
    | "Inline" ^^ {(_,_) => DefaultInline}
    )

  lazy val assum_list : Parser[List[(Boolean,(List[IdentDecl],ConstrExpr))]] = (
    assum_coe.++ ^^ {(_,bl) => bl}
    | simple_assum_coe ^^ {(_,b:Boolean,ids:List[IdentDecl],c:ConstrExpr) =>
      List((b,(ids,c)))
    }
    )

  lazy val assum_coe : Parser[(Boolean,(List[IdentDecl],ConstrExpr))] = "(" ~ simple_assum_coe ~ ")" ^^ {
    (_:List[Line],
     _:String,
     b:Boolean,
     tu:(List[IdentDecl],ConstrExpr),
    _:String) =>
      (b,tu)
  }
  lazy val simple_assum_coe : Parser[(Boolean,(List[IdentDecl],ConstrExpr))] = ident_decl.++ ~
    of_type_with_opt_coercion ~ termParse.lconstr ^^ { (_,udes,b,c) =>
    (b match {case Some(_) => true case None => false},(udes,c))
  }

  lazy val constructor_type: Parser[kernelTypes.id.T => (Boolean, (kernelTypes.id.T, ConstrExpr))] =
    termParse.binders ~? (
      of_type_with_opt_coercion ~ termParse.lconstr ^^ {(_,coe,c) => l:List[LocalBinderExpr] => id:kernelTypes.id.T => (coe.isDefined,(id,interpTypes.HVals.mkProdCN(l)(c)))}
      ) ^^ {
      (_,l,oT) => oT match {
        case Some(t) => t(l)
          //TODO: Why does lambda expression not work here?
        case None => constrTypeNoneCase(l)
      }
    }

  val constrTypeNoneCase : List[LocalBinderExpr] => kernelTypes.id.T => (Boolean,(kernelTypes.id.T,ConstrExpr)) = l => id =>
    (false,(id,interpTypes.HVals.mkProdCN(l)(Hole(None,IntroAnonymous,None))))

  //TODO:Implement
  lazy val constructor: Parser[(Boolean, (kernelTypes.id.T, ConstrExpr))] = primParse.identref ~ constructor_type ^^ { (_, id, c) => c(id)}

  lazy val of_type_with_opt_coercion : Parser[Option[Boolean]] = (
    ":>>" ^^ {(_,_) => Some(false)}
    | ":>" ^^ {(_,_) => Some(true)}
    | ":" ^^ {(_,_) => None}
  )

  lazy val ident_decl : Parser[IdentDecl] = primParse.identref ~? univ_decl ^^ { (_, i, l) => (i,l)}

  lazy val univ_decl : Parser[UniverseDeclExpr] =
    helper.univ_decl_prefix ~ helper.univ_decl_cs ^^{
      (_:List[Line], ids:List[model.kernelTypes.id.T], b:Boolean, cs:List[GlobConstraint], b2:Boolean) => {
        GenUniverseDecl(ids, b, cs, b2)
      }
    }

  lazy val univ_constraint : Parser[GlobConstraint]=
    termParse.universe_level ~ ("<" ^^ {(_,_) => Lt} | "=" ^^ {(_,_) => Eq} | "<=" ^^ {(_,_) => Le}) ~
      termParse.universe_level ^^ {(_,l,ord,r) =>
      (l,ord,r)
    }

  object helper {
    lazy val univ_dcl_univ_constraints_helper : Parser[String] = "|" ^^ {(_,s) => s}
    lazy val univ_helper : Parser[String] = "@{" ^^ {(_,s) => s}
    lazy val pipe_helper : Parser[String] = "|" ^^ {(_,s) => s}
    lazy val plus_helper : Parser[String] = "+" ^^ {(_,s) => s}

    lazy val univ_decl_prefix : Parser[(List[model.kernelTypes.id.T],Boolean)] = helper.univ_helper ~?
      primParse.identref.++ ~? helper.plus_helper ^^ { (_,_,ids,os) =>
      (ids.getOrElse(Nil),os match {case Some(_) => true case None => false})
    }

    lazy val univ_decl_cs : Parser[(List[GlobConstraint],Boolean)] =  (helper.pipe_helper ~?
      (univ_constraint ~? ("," ~ univ_constraint).++) ~? "+" ~ "}" ^^ {(_, _, uCs, s, _) => {
      uCs match {
        case Some(_uCs) =>
          val lUc = _uCs._1
          val rUcs = _uCs._2 match {
            case Some(_rUcs) => _rUcs.map(_._2)
            case None => Nil
          }
          (lUc :: rUcs, s match {case Some(_) => true case None => false} )
        case None => (Nil, s match {case Some(_) => true case None => false})}}
    }
    | ("}" ^^ {(_,_) => true} | "|}" ^^ {(_,_) => false}) ^^ {(_,b) => (Nil,b)}
      )

    object gallina {
      val name_of_ident_decl : IdentDecl => NameDecl = i =>  {
        (Name(i._1),i._2)
      }

      val name_of_ident : model.kernelTypes.id.T => Name = id => {
        Name(id)
      }
      lazy val theorem_helper : Parser[((model.kernelTypes.id.T,Option[UniverseDeclExpr]),
        (List[LocalBinderExpr],
          ConstrExpr))] = "with" ~ ident_decl ~ termParse.binders ~
        ":" ~ termParse.lconstr ^^ { (_,_,id,bs,_,c ) => {
          (id,(bs,c))
        }
      }
    }
  }
}
