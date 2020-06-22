package parsebackParse

import model.engineTypes.evarKinds
import model.engineTypes.namegen.{IntroAnonymous, IntroFresh, IntroIdentifier}
import model.interpTypes.HVals.{aliasvar, mkFix, mkLambdaCN, mkProdCN, mkSingleFix}
import model.interpTypes.constrExpr.{BindingKind, Default, ExplByName, Explication, Explicit, Generalized, Implicit, InstanceExpr, Numeral, SString}
import model.interpTypes.constrExpr.constructor.{Cases, CasesPatternExpr, Cast, CoFix, ConstrExpr, Evar, Fix, FixType, Hole, If, LetIn, LetTuple, LocalAssum, LocalBinderExpr, LocalDef, LocalPattern, MeasureRec, PatCast, Prim, RecursionOrderExpr, Ref, Sort, StructRec, WfRec}
import model.kernelTypes
import model.kernelTypes.constr.LetPatternStyle
import model.kernelTypes.id
import model.kernelTypes.name.Name
import model.libraryTypes.libNames.Qualid
import model.pretypingTypes.{CastConv, globTerm}
import model.pretypingTypes.globTerm.GlobLevel
import parseback._
import implicits._
import ParserFix._

import scala.collection.mutable

object fixedTermParse {

  implicit val W:parseback.Whitespace = Whitespace("""\s+""".r.?)

  object oprConstr {

    implicit val W:parseback.Whitespace = Whitespace("""\s+""".r.?)

    /**
      * Mapping for rules to ints
      */
    val ruleIntMapping: mutable.HashMap[Int, Parser[ConstrExpr]] = scala.collection.mutable.HashMap(
      (200,oprConstr_200.parse),
      199 -> oprConstr_199.parse,
      198 -> oprConstr_198.parse,
      197 -> oprConstr_197.parse,
      196 -> oprConstr_196.parse,
      195 -> oprConstr_195.parse,
      194 -> oprConstr_194.parse,
      193 -> oprConstr_193.parse,
      192 -> oprConstr_192.parse,
      191 -> oprConstr_191.parse,
      190 -> oprConstr_190.parse,
      189 -> oprConstr_189.parse,
      188 -> oprConstr_188.parse,
      187 -> oprConstr_187.parse,
      186 -> oprConstr_186.parse,
      185 -> oprConstr_185.parse,
      184 -> oprConstr_184.parse,
      183 -> oprConstr_183.parse,
      182 -> oprConstr_182.parse,
      181 -> oprConstr_181.parse,
      180 -> oprConstr_180.parse,
      179 -> oprConstr_179.parse,
      178 -> oprConstr_178.parse,
      177 -> oprConstr_177.parse,
      176 -> oprConstr_176.parse,
      175 -> oprConstr_175.parse,
      174 -> oprConstr_174.parse,
      173 -> oprConstr_173.parse,
      172 -> oprConstr_172.parse,
      171 -> oprConstr_171.parse,
      170 -> oprConstr_170.parse,
      169 -> oprConstr_169.parse,
      168 -> oprConstr_168.parse,
      167 -> oprConstr_167.parse,
      166 -> oprConstr_166.parse,
      165 -> oprConstr_165.parse,
      164 -> oprConstr_164.parse,
      163 -> oprConstr_163.parse,
      162 -> oprConstr_162.parse,
      161 -> oprConstr_161.parse,
      160 -> oprConstr_160.parse,
    )

    /**
      * Short-type for tuples (precedence, parser) stored in a Map
      */
    type ParserTable = scala.collection.mutable.Map[Int,Parser[ConstrExpr]]

    val notationContainer:ParserTable = scala.collection.mutable.HashMap.empty

    trait BaseOpr {
      val level : Int
      val parse : Parser[ConstrExpr] = baseParser
      val baseParser: Parser[ConstrExpr] = notationContainer(level) | ruleIntMapping(level-1)
    }

    lazy val oprConstr_200: BaseOpr = new BaseOpr {
      override val level: Int = 200
      override val parse: Parser[ConstrExpr] = binder_constr | baseParser
    }
    lazy val oprConstr_199: BaseOpr = new BaseOpr {
      override val level: Int = 199
    }
    lazy val oprConstr_198: BaseOpr =  new BaseOpr {
      override val level: Int = 198
    }
    lazy val oprConstr_197: BaseOpr = new BaseOpr {
      override val level: Int = 197
    }
    lazy val oprConstr_196: BaseOpr = new BaseOpr {
      override val level: Int = 196
    }
    lazy val oprConstr_195: BaseOpr = new BaseOpr {
      override val level: Int = 195
    }
    lazy val oprConstr_194: BaseOpr = new BaseOpr {
      override val level: Int = 194
    }
    lazy val oprConstr_193: BaseOpr = new BaseOpr {
      override val level: Int = 193
    }
    lazy val oprConstr_192: BaseOpr = new BaseOpr {
      override val level: Int = 192
    }
    lazy val oprConstr_191: BaseOpr = new BaseOpr {
      override val level: Int = 191
    }
    lazy val oprConstr_190: BaseOpr = new BaseOpr {
      override val level: Int = 190
    }
    lazy val oprConstr_189: BaseOpr = new BaseOpr {
      override val level: Int = 189
      
    }
    lazy val oprConstr_188: BaseOpr = new BaseOpr {
      override val level: Int = 188
      
    }
    lazy val oprConstr_187: BaseOpr = new BaseOpr {
      override val level: Int = 187
      
    }
    lazy val oprConstr_186: BaseOpr = new BaseOpr {
      override val level: Int = 186
      
    }
    lazy val oprConstr_185: BaseOpr = new BaseOpr {
      override val level: Int = 185
      
    }
    lazy val oprConstr_184: BaseOpr = new BaseOpr {
      override val level: Int = 184
      
    }
    lazy val oprConstr_183: BaseOpr = new BaseOpr {
      override val level: Int = 183
      
    }
    lazy val oprConstr_182: BaseOpr = new BaseOpr {
      override val level: Int = 182
      
    }
    lazy val oprConstr_181: BaseOpr = new BaseOpr {
      override val level: Int = 181
      
    }
    lazy val oprConstr_180: BaseOpr = new BaseOpr {
      override val level: Int = 180
      
    }
    lazy val oprConstr_179: BaseOpr = new BaseOpr {
      override val level: Int = 179
      
    }
    lazy val oprConstr_178: BaseOpr = new BaseOpr {
      override val level: Int = 178
      
    }
    lazy val oprConstr_177: BaseOpr = new BaseOpr {
      override val level: Int = 177
      
    }
    lazy val oprConstr_176: BaseOpr = new BaseOpr {
      override val level: Int = 176
      
    }
    lazy val oprConstr_175: BaseOpr = new BaseOpr {
      override val level: Int = 175
      
    }
    lazy val oprConstr_174: BaseOpr = new BaseOpr {
      override val level: Int = 174
      
    }
    lazy val oprConstr_173: BaseOpr = new BaseOpr {
      override val level: Int = 173
      
    }
    lazy val oprConstr_172: BaseOpr = new BaseOpr {
      override val level: Int = 172
      
    }
    lazy val oprConstr_171: BaseOpr = new BaseOpr {
      override val level: Int = 171
      
    }
    lazy val oprConstr_170: BaseOpr = new BaseOpr {
      override val level: Int = 170
      
    }
    lazy val oprConstr_169: BaseOpr = new BaseOpr {
      override val level: Int = 169
      
    }
    lazy val oprConstr_168: BaseOpr = new BaseOpr {
      override val level: Int = 168
      
    }
    lazy val oprConstr_167: BaseOpr = new BaseOpr {
      override val level: Int = 167
      
    }
    lazy val oprConstr_166: BaseOpr = new BaseOpr {
      override val level: Int = 166
      
    }
    lazy val oprConstr_165: BaseOpr = new BaseOpr {
      override val level: Int = 165
      
    }
    lazy val oprConstr_164: BaseOpr = new BaseOpr {
      override val level: Int = 164
      
    }
    lazy val oprConstr_163: BaseOpr = new BaseOpr {
      override val level: Int = 163
      
    }
    lazy val oprConstr_162: BaseOpr = new BaseOpr {
      override val level: Int = 162
      
    }
    lazy val oprConstr_161: BaseOpr = new BaseOpr {
      override val level: Int = 161
      
    }
    lazy val oprConstr_160: BaseOpr = new BaseOpr {
      override val level: Int = 160
      
    }
  }

  lazy val global : Parser[Qualid] = primParse.reference

  lazy val binders : Parser[List[LocalBinderExpr]] = binder.++ ^^ {(_,bs) => bs.flatten}

  lazy val binder_constr : Parser[ConstrExpr]  = (
    "forall" ~ open_binders ~ "," ~ oprConstr.oprConstr_200.parse ^^ {(_,_,ob,_,oc) => mkProdCN (ob)(oc)}
      | "fun" ~ open_binders ~ "=>" ~ oprConstr.oprConstr_200.parse ^^ {(_,_,ob,_,oc) => mkLambdaCN (ob)(oc)}
      | "let" ~ primParse.name ~? binders ~? opt_type_cstr ~ ":=" ~ oprConstr.oprConstr_200.parse ~
      "in" ~ oprConstr.oprConstr_200.parse ^^ {
      (_,tu,_,c1,_,c2) => {
        val id = tu._1._1._2
        val bl = tu._1._2
        val ty = tu._2
        //TODO: Discuss how tuple Notation (_,None), { CAst.v = CCast(c, CastConv t) } fits to returntype of type_cstr which should be Option[ConstrExpr] and not a tuple
        val tub = ty match {
          case None =>
            c1 match {
              case Cast(c,CastConv(t)) => (Some(t),c)
              case _ => (None,c1)
            }
          case _ => (ty.get,c1)
        }
        bl match {
          //TODO: In the original file, ty is a tuple, oprConstr probably leads back to AST construction
          case Some(_bl) => LetIn(id,mkLambdaCN(_bl)(tub._2),tub._1.map(mkProdCN(_bl)),c2)
          case None => LetIn(id,mkLambdaCN(Nil)(tub._2),tub._1.map(mkProdCN(Nil)),c2)
        }
      }
    }
      | "let" ~ single_fix ~ "in" ~ oprConstr.oprConstr_200.parse ^^ {
      (_:List[Line],_:String,fixb:Boolean,fixr:(id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr]),_:String,c:ConstrExpr) => {
        val fixp = mkSingleFix(fixb)(fixr)
        val id = fixp match {
          case Fix(id1,_) => id1
          case CoFix(id1,_) => id1
          //TODO: Throw meaningful excepetions
          case _ => throw new IllegalStateException("Should be Fix or CoFix")
        }
        LetIn(Name(id),fixp,None,c)
      }
    }
      | "let" ~ "(" ~? (primParse.name ~? ("," ~ primParse.name).++) ~ ")" ~? return_type ~
      ":=" ~ oprConstr(200) ~ "in" ~ oprConstr(200) ^^ {
      (_:List[Line],
       tu:((((String,String),Option[(kernelTypes.name.T,Option[List[(String,kernelTypes.name.T)]])]),String),Option[(Option[kernelTypes.name.T],Option[ConstrExpr])]),
       _:String,
       c1:ConstrExpr,
       _:String,
       c2:ConstrExpr) => {
        val names = tu._1._1._2 match {
          case Some(ns) => ns._1 :: ns._2.getOrElse(Nil).map(n => n._2)
          case None => Nil
        }
        val rt = tu._2
        rt match {
          case Some(_rt) => LetTuple (names, _rt, c1, c2)
          case None => LetTuple (names,(None,None),c1,c2)
        }
      }
    }
      | "let" ~ "'" ~ pattern(200) ~ ":=" ~ oprConstr(200) ~
      "in" ~ oprConstr(200) ^^ {
      (_:List[Line],
       tu:((String,String),CasesPatternExpr),
       _:String,
       c1:ConstrExpr,
       _:String,
       c2:ConstrExpr
      ) => {
        val pat = tu._2
        Cases(LetPatternStyle,None,List((c1,None,None)),List((List(List(pat)),c2)))
      }
    }
      | "let" ~ "'" ~ pattern(200) ~ ":=" ~ oprConstr(200) ~
      case_type ~ "in" ~ oprConstr(200) ^^ {
      (_:List[Line],
       tu:(((String,String),CasesPatternExpr),String),
       ct:ConstrExpr,
       c1:ConstrExpr,
       _:String,
       c2:ConstrExpr) => Cases(LetPatternStyle,Some(ct),List((c1,None,None)),List((List(List(tu._1._2)),c2)))
    }
      | "let" ~ "'" ~ pattern(200) ~ "in" ~ pattern(200) ~ ":=" ~
      oprConstr(200) ~ case_type ~ "in" ~ oprConstr(200) ^^ {
      (
        _:List[Line],
        tu:(((((String,String),CasesPatternExpr),String),CasesPatternExpr),String),
        ct:ConstrExpr,
        c1:ConstrExpr,
        _:String,
        c2:ConstrExpr
      ) => {
        val pat1 = tu._1._1._1._2
        val pat2 = tu._1._2
        Cases(LetPatternStyle,Some(ct),List((c1,aliasvar(pat1),Some(pat2))),List((List(List(pat1)),c2)))
      }
    }
      | "if" ~ oprConstr.oprConstr_200.parse~? return_type ~ "then" ~
      oprConstr.oprConstr_200.parse ~ "else" ~ oprConstr.oprConstr_200.parse ^^ {
      (_:List[Line],
       tu:((String,ConstrExpr),Option[(Option[kernelTypes.name.T],Option[ConstrExpr])]),
       _:String,
       c1:ConstrExpr,
       _:String,
       c2:ConstrExpr) => {
        If(tu._1._2,tu._2 match {case Some(x) => x case None => (None,None)},c1,c2)
      }
    }
      | fix_constr
    )

  /**
    * References rule arg from reference manual:
    *
    * {{{
    *   arg          ::=  term
    *                   | ( ident := term )
    * }}}
    */
  lazy val appl_arg:Parser[(ConstrExpr,Option[Explication])]   = (
    "(" ~ primParse.ident ~ ":=" ~ lconstr ~ ")" ^^ { (_, _, i, _, c, _) => (c,Some(ExplByName(i)))}
      | oprConstr(9) ^^ {(_,c) => (c,None)}
    )

  lazy val lconstr : Parser[ConstrExpr] = oprConstr.oprConstr_200.parse

  lazy val single_fix:Parser[(Boolean,(id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr]))] = fix_kw ~ fix_decl ^^ { (_, kw, dcl)=>(kw,dcl)}

  lazy val fix_decl:Parser[(id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr])] =
    primParse.identref ~? opt_binders_fixannot ~? opt_type_cstr ~ ":=" ~ oprConstr.oprConstr_200.parse ^^ {
      (_,id,fixb,ty,_,c) => {
        fixb match {
          case Some(_fixb) => (id, _fixb._1,_fixb._2, c,ty match {case Some(_ty) => _ty case None => None})
          case None => (id,Nil,None,c,ty match {case Some(_ty) => _ty case None => None})
        }
      }
    }

  /**
    * Atomic values for term. In Detail:
    *
    * {{{
    *     | qualid
    *     | sort
    *     | num
    *     | _
    * }}}
    *
    * Rules beginning with Questionmark are not referenced but used in g_constr.mlg
    */
  lazy val atomic_constr : Parser[ConstrExpr] = (
    global ~? instance ^^ {
      (_,g,i) => Ref(g,i match {case Some(_i) => _i case None => None})
    }
      | sort ^^ {
      (_,s) => Sort(s)
    }
      | primParse.int ^^ {
      (_,i) => Prim(Numeral(i,s = true))
    }
      | primParse.string ^^ {
      (_,s) => Prim(SString(s))
    }
      | "_" ^^ {
      (_,_) => Hole(None,IntroAnonymous,None)
    }
      | "?" ~ "[" ~ primParse.ident ~ "]" ^^ {
      (_,_,_,i,_) => Hole(None,IntroIdentifier(i),None)
    }
      | "?" ~ "[" ~ primParse.pattern_ident ~ "]" ^^ {
      (_,_,_,i,_) => Hole(None,IntroFresh(i),None)
    }
      | primParse.pattern_ident ~? evar_instance ^^ {
      (_,i,e) => {
        Evar(i,e match { case Some(x) => x.map(h => (h.id,h.c)) case None => Nil})
      }
    }
    )

  /**
    * unreferenced rule used in g_constr.mlg
    *
    * instanceHelper is used here, because implicit conversion of @{ in combination with ~? is not possible
    */
  lazy val instance:Parser[Option[InstanceExpr]] =
    instanceHelper ~? universe_level.++ ~ "}" ^^ { (_,_,l,_) => Some(l.getOrElse(Nil))}

  lazy val instanceHelper : Parser[String] = "@{" ^^ {(_,s) => s}

  lazy val universe_level : Parser[GlobLevel] = (
    "Set" ^^ {(_,_) => globTerm.global.Set}
      |"Prop" ^^ {(_,_) => globTerm.global.Prop}
      |  "Type" ^^ {(_,_) => globTerm.global.Type(globTerm.universe.Unknown)}
      |  "_" ^^ {(_,_) => globTerm.global.Type(globTerm.universe.Anonymous)}
      |  global ^^ {(_,id) => globTerm.global.Type(globTerm.universe.Named(id))}
    )

  /**
    * Defines Fix- and Cofix-rules in a combination of '''fix_bodies''' and '''cofix_bodies'''
    *
    * In Detail:
    * {{{
    *   term        ::= fix fix_bodies
    *                   | cofix cofix_bodies
    *   fix_bodies   ::=  fix_body
    *                   | fix_body with fix_body with … with fix_body for ident
    * cofix_bodies ::=  cofix_body
    *                   | cofix_body with cofix_body with … with cofix_body for ident
    * }}}
    */
  lazy val fix_constr : Parser[FixType] = (
    single_fix ^^ {
      (
        _:List[Line],
        fixb:Boolean,
        fixr:(id.T,List[LocalBinderExpr], Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr])
      ) => mkSingleFix(fixb)(fixr)
    }
      | single_fix ~ ("with" ~ fix_decl).++ ~ "for" ~ primParse.identref ^^
      {(_:List[Line],
        sfixb:Boolean,sfixr:(kernelTypes.id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr]),
        sdcls:List[(String,(kernelTypes.id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr]))],
        _:String,
        id:kernelTypes.id.T) => mkFix(sfixb)(id)(sfixr :: sdcls.map(sdcl => sdcl._2))}
    )

  /**
    * Used for Fix and Cofix construction
    */
  lazy val fix_kw : Parser[Boolean] = (
    "fix" ^^ {(_,_) => true}
      |"cofix" ^^ {(_,_) => false}
    )

  lazy val record_pattern : Parser[(Qualid,CasesPatternExpr)] =
    global ~ ":=" ~ pattern(200) ^^ { (_,q,_,p) => (q,p)}


  lazy val record_patterns : Parser[List[(Qualid,CasesPatternExpr)]]= (
    record_pattern ~ ";" ~? record_patterns ^^ {
      (_:List[Line],
       rp1q:Qualid,
       rp1c:CasesPatternExpr,
       _:String,
       rps:Option[List[(Qualid,CasesPatternExpr)]]) => {
        rps match {
          case Some(_rps) => (rp1q,rp1c) :: _rps
          case None => (rp1q,rp1c) :: Nil
        }
      }
    }
      |record_pattern ~ ";" ^^ {(_:List[Line],rpq:Qualid,rpc:CasesPatternExpr,_:String) => List((rpq,rpc))}
      |record_pattern ^^ {(_,rpq,rpc) => List((rpq,rpc))}
    )

  lazy val impl_ident_tail : Parser[Name => LocalBinderExpr] = (
    "}" ^^ {(_,_) => HFuncs.binder_of_name(Implicit)(_)}
      |primParse.name.++ ~ ":" ~ lconstr ~ "}" ^^ { (_, nal, _, c, _) => (n:Name) => LocalAssum(n::nal,Default(Implicit),c)}
      |primParse.name.++ ~ "}" ^^ { (_, nal, _) => (n:Name) => LocalAssum(n::nal,Default(Implicit),Hole(Some(evarKinds.BinderType(n)),IntroAnonymous,None))}
      | ":" ~ lconstr ~ "}" ^^ {(_,_,c,_) => (n:Name) => LocalAssum(List(n),Default(Implicit),c)}
    )

  /**
    * References annotation rule from the reference manual. However,
    * sub-rules regarding wf and measure are not mentioned in the reference
    * manual
    */
  lazy val fixannot : Parser[RecursionOrderExpr] = (
    "{" ~  "struct" ~ primParse.identref ~ "}" ^^ { (_, _, _, id, _) => StructRec(id)}
      |"{" ~ "wf" ~ constr ~ primParse.identref ~ "}" ^^ { (_, _, _, c, id, _) => WfRec(id,c)}
      |"{" ~ "measure" ~ constr ~? primParse.identref ~? constr ~ "}" ^^ { (_, _, c, oId, oC, _) => MeasureRec(oId,c,oC)}
    )

  /**
    * binders [annotation] | binders | Epsilon
    */
  //TODO:potentially epsilon with (Nil,None) as result
  lazy val opt_binders_fixannot : Parser[(List[LocalBinderExpr],Option[RecursionOrderExpr])] = (
    impl_name_head ~ impl_ident_tail ~ opt_binders_fixannot ^^ {
      (_:List[Line],na:kernelTypes.name.Name,assum:kernelTypes.name.Name => LocalBinderExpr,bfbs:List[LocalBinderExpr],bfr:Option[RecursionOrderExpr]) =>
        (assum(na) :: bfbs,bfr)
    }
      |fixannot ^^ {(_:List[Line],fix:RecursionOrderExpr) => (Nil,Some(fix))}
      |binder ~ opt_binders_fixannot ^^ { (_:List[Line], b:List[LocalBinderExpr], bfixbs:List[LocalBinderExpr], bfixr:Option[RecursionOrderExpr]) =>
      (b ++ bfixbs,bfixr)
    }
    )

  /**
    * Representing Binders not parenthesized
    */
  lazy val open_binders : Parser[List[LocalBinderExpr]] = (
    primParse.name.++ ~ ":" ~ lconstr ^^ {
      (_,ids,_,c) => {
        List(LocalAssum(ids,Default(Explicit),c)).asInstanceOf[List[LocalBinderExpr]]
      }
    }
      //TODO: This is unambiguous because names can be parsed before applying binders rule or after applying binders rule, however same parse trees derive
      | primParse.name.++ ~? binders ^^ {
      (_,ids,bs) => {
        bs match {
          case Some(_bs) =>  HFuncs.binders_of_names(ids) ++ _bs
          case None =>  HFuncs.binders_of_names(ids)
        }
      }
    }
      | primParse.name ~ ".." ~ primParse.name ^^ {
      (_,n1,_,n2) => {
        List(LocalAssum(List(n1, kernelTypes.name.Name(".."), n2),Default(Explicit),Hole(None,IntroAnonymous,None)))
      }
    }
      | closed_binder ~? binders ^^ {(_,cBs,bs) =>
      bs match {
        case Some(_bs) => cBs ++ _bs
        case None => cBs
      }
    }
    )

  lazy val binder : Parser[List[LocalBinderExpr]] = (
    primParse.name ^^ { (_, id) => {
      List(LocalAssum(List(id),Default(Explicit),Hole(None,IntroAnonymous,None)))
        .asInstanceOf[List[LocalBinderExpr]]}
    }
      | closed_binder ^^ {(_,cl) => cl}
    )

  /**
    * Representing parenthesized binders
    */
  lazy val closed_binder : Parser[List[LocalBinderExpr]] = (
    "(" ~ primParse.name ~ primParse.name.++ ~ ":" ~ lconstr ~ ")" ^^ {
      (_,tu,ids,_,c,_) => {
        List(LocalAssum(tu._2 :: ids,Default(Explicit),c))
          .asInstanceOf[List[LocalBinderExpr]]
      }}
      | "(" ~ primParse.name ~ ":" ~ lconstr ~ ")" ^^ {
      (_,_,id,_,c,_) => {
        List(LocalAssum(List(id),Default(Explicit),c))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "(" ~ primParse.name ~ ":=" ~ lconstr ~ ")" ^^ {
      (_,_,id,_,c,_) => {
        c match {
          case Cast(c1, CastConv(t)) =>
            List(LocalDef(id,c1,Some(t)))
              .asInstanceOf[List[LocalBinderExpr]]
          case _ =>
            List(LocalDef(id,c,None))
              .asInstanceOf[List[LocalBinderExpr]]
        }
      }
    }
      | "(" ~ primParse.name ~ ":" ~ lconstr ~ ":=" ~ lconstr ~ ")" ^^ {
      (_,tr,t,_,c,_) => {
        List(LocalDef(tr._1._2,c,Some(t)))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "{" ~ primParse.name ~ "}" ^^ {
      (_,_,id,_) => {
        List(LocalAssum(List(id),Default(Implicit),Hole(None,IntroAnonymous,None)))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "{" ~ primParse.name ~ primParse.name.++ ~ ":" ~ lconstr ~ "}" ^^ {
      (_,tu,ids,_,c,_) => {
        List(LocalAssum(tu._2 :: ids,Default(Implicit),c))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "{" ~ primParse.name ~ ":" ~ lconstr ~ "}" ^^ {
      (_,_,id,_,c,_) => {
        List(LocalAssum(List(id),Default(Implicit),c))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "{" ~ primParse.name ~ primParse.name.++ ~ "}" ^^ {
      (_,_,id,ids,_) => {
        (id :: ids)
          .map(id => LocalAssum(List(id),Default(Implicit),Hole(None,IntroAnonymous,None)))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "`(" ~ typeclass_constraint ~? ("," ~ typeclass_constraint).++ ~ ")" ^^ {
      (_,_,tC,tCs,_) => {
        //List only containing typeclass constraints ignoring the Comma-String
        val tClConsts = tC :: tCs.getOrElse(Nil).map(tu => tu._2)
        tClConsts.map(hTCC => LocalAssum(List(hTCC.id), Generalized(Implicit,Explicit,hTCC.b),hTCC.c))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "`{" ~ typeclass_constraint ~? ("," ~ typeclass_constraint).++ ~ "}" ^^ {
      (_,_,tC,tCs,_) => {
        //List only containing typeclass constraints ignoring the Comma-String
        val tClConsts = tC :: tCs.getOrElse(Nil).map(tu => tu._2)
        tClConsts.map(hTCC => LocalAssum(List(hTCC.id), Generalized(Implicit,Implicit,hTCC.b),hTCC.c))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "'" ~ pattern(0) ^^ {
      (_,_,p) => {
        val tu : (CasesPatternExpr,Option[ConstrExpr]) = p match {
          case PatCast(p1,ty) => (p1,Some(ty))
          case _ => (p,None)
        }
        List(LocalPattern((tu._1,tu._2)))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }

    )

  lazy val typeclass_constraint : Parser[HTypes.HTypeClConst]= (
    "!" ~ oprConstr(200) ^^ {
      (_,_,c) => {
        HTypes.HTypeClConst(kernelTypes.name.Anonymous,b = true,c)
      }
    }
      | "{" ~ primParse.name ~ "}" ~ ":" ~? "!" ~ oprConstr(200) ^^ {
      (_,tu,_,_,oExc,c) => {
        oExc match {
          case Some(_) => HTypes.HTypeClConst(tu._2,b = true,c)
          case None => HTypes.HTypeClConst(tu._2,b = false,c)
        }
      }
    }
      | name_colon ~? "!" ~ oprConstr(200) ^^ {
      (_,nC,oExc,c) => {
        oExc match {
          case Some(_) => HTypes.HTypeClConst(nC,b = true,c)
          case None => HTypes.HTypeClConst(nC,b = false,c)
        }
      }
    }
      | oprConstr(200) ^^ {(_,c) => HTypes.HTypeClConst(kernelTypes.name.Anonymous,b = false,c)}
    )

  /**
    * Used for optional type annotation. Type-Annotation can be any term
    * on level 200
    */
  //TODO:potentially epsilon with None as result
  lazy val opt_type_cstr : Parser[Option[ConstrExpr]] =
  ":" ~ lconstr ^^ {(_,_,c) => Some(c)}


  lazy val name_colon : Parser[kernelTypes.name.T] = (
    primParse.ident ~ ":" ^^ {
      (_,id,_) => {
        kernelTypes.name.Name(id)
          .asInstanceOf[kernelTypes.name.T]
      }
    }
      | "_" ~ ":" ^^ {
      (_,_,_) => {
        kernelTypes.name.Anonymous
      }
    }
    )

  /**
    * Helper types for AST-construction. Not included in the final AST
    */
  object HTypes {
    sealed case class HTypeClConst(id:kernelTypes.name.T,
                                   b:Boolean,
                                   c:ConstrExpr)
    sealed case class HInst(id:kernelTypes.id.T, c:ConstrExpr)
    sealed case class HUniverseExpr(id:kernelTypes.id.T, i:Int)
  }

  object HFuncs {
    def binders_of_names(l:List[kernelTypes.name.T]):List[LocalBinderExpr] = {
      l.map(binder_of_name(Explicit))
    }

    def binder_of_name(expl:BindingKind)(name:kernelTypes.name.T):LocalBinderExpr = {
      LocalAssum(List(name),Default(expl),Hole(Some(evarKinds.BinderType(name)),IntroAnonymous,None))
    }
  }

}
