package parsebackParse

import parseback._
import implicits._
import ParserFix._
import ParserHelper._
import model.gramlibTypes.gramext.{LeftA, NonA, RightA}
import model.interpTypes.constrExpr.{InConstrEntry, InCustomEntry}
import model.interpTypes.constrExpr.constructor.ConstrExpr
import model.interpTypes.notationTerm
import model.interpTypes.notationTerm.{AsIdent, AsIdentOrPattern, AsStrictPattern, ETBigint, ETBinder, ETConstr, ETGlobal, ETIdent, ETPattern}
import model.kernelTypes
import model.libTypes.flags.{CompatVersion, Current, V8_8, V8_9}
import model.parsingTypes.extend
import model.parsingTypes.extend.{NextLevel, NumLevel, ProductionLevel}
import model.vernacTypes.vernacExpr
import model.vernacTypes.vernacExpr.{SetAssoc, SetCompatVersion, SetCustomEntry, SetEntryType, SetFormat, SetItemLevel, SetLevel, SetOnlyParsing, SetOnlyPrinting}

object syntaxParse {
  implicit val W : parseback.Whitespace = parseback.Whitespace("""\s+""".r.?)

  lazy val syntax: Parser[vernacExpr.VernacExpr] = (
    ("Notation" ~> primParse.identref ~? primParse._ident.++ ~ (":= " ~> termParse.constr ~? opt_only_parsing ^^ { (_,c,cV) => (c,cV) })) ^^ {
      (_:List[Line],id:kernelTypes.id.T,optIds:Option[List[String]],c:ConstrExpr,comp:Option[Option[CompatVersion]]) =>
        vernacExpr.SyntacticDefinition(id,(optIds.getOrElse(Nil),c),comp.flatten)
    }
    | "Notation" ~> primParse.string ~ (":=" ~> termParse.constr ~? ("(" ~> list1WithSeperator(syntax_modifier,",") <~ ")") ~? (":" ~> primParse._ident)) ^^ {
      (_,s,c,oSyntaxMods,sc) => vernacExpr.Notation(c,(s,oSyntaxMods.getOrElse(Nil)),sc)
    }
    | "Format" ~> "Notation" ~> primParse.string ~ primParse.string ~ primParse.string ^^ vernacExpr.NotationAddFormat
    | "Reserved" ~> "Notation" ~>primParse.ne_string ~? ("(" ~> list1WithSeperator(syntax_modifier,",") <~ ")") ^^ {
      (_,s,ssMods) => vernacExpr.SyntaxExtension(b = false,(s,ssMods.getOrElse(Nil)))
    }
  )

  lazy val syntax_modifier: Parser[vernacExpr.SyntaxModifier] = (
    "at" ~> "level" ~> primParse.natural ^^ SetLevel
    | "in" ~> "custom" ~> primParse._ident ^^ {(_,i) => SetCustomEntry(i,None)}
    | "in" ~> "custom" ~> primParse._ident ~ ("at" ~> "level" ~> primParse.natural) ^^ {(_,i,n) => SetCustomEntry(i,Some(n))}
    | "left" ~> "associativity" ^^ SetAssoc(LeftA)
    | "right" ~> "associativity" ^^ SetAssoc(RightA)
    | "no" ~> "associativity" ^^ SetAssoc(NonA)
    | "only" ~> "printing" ^^ SetOnlyPrinting
    | "only" ~> "parsing" ^^ SetOnlyParsing
    | "compat" ~> primParse.string ^^ {(_,s) => SetCompatVersion(helper.parseCompatVersion(s))}
    | "format" ~> primParse.string ~? primParse.string ^^ {(_,s1,s2) => s2 match {case Some(s) => SetFormat(s1,s) case None => SetFormat("text",s1)}}
    | primParse._ident ~ ("," ~> list1WithSeperator(primParse._ident,",") ~ ("at" ~> level)) ^^ {
      (_,id,ss,level) => SetItemLevel(id::ss,None,Some(level))
    }
    | primParse._ident ~ ("at" ~> level) ^^ {(_,id,level) => SetItemLevel(List(id),None,Some(level))}
    | primParse._ident ~ ("at" ~> level ~ constr_as_binder_kind) ^^ {(_,i,l,c) => SetItemLevel(List(i),Some(c),Some(l))}
    | primParse._ident ~ constr_as_binder_kind ^^ {(_,i,b) => SetItemLevel(List(i),Some(b),None)}
    | primParse._ident ~ syntax_extension_type ^^ {(_,i,s) => SetEntryType(i,s)}
  )

  lazy val syntax_extension_type: Parser[notationTerm.ConstrEntryKeyGen[Option[ProductionLevel]]] = (
    "ident" ^^ ETIdent
    | "global" ^^ ETGlobal
    | "bigint" ^^ ETBigint
    | "binder" ^^ ETBinder(isOpenBindersList = true)
    | "constr" ^^ ETConstr(InConstrEntry,None,None)
    | ("constr":Parser[String]) ~?> at_level ~? constr_as_binder_kind ^^ {(_,n,b) => ETConstr(InConstrEntry,b,n)}
    | "pattern" ^^ ETPattern(isStrictPattern = false,None)
    | "pattern" ~> "at" ~> "level" ~> primParse.natural ^^ {(_,n) => ETPattern(isStrictPattern = false,Some(n))}
    | "strict" ~> "pattern" ^^ ETPattern(isStrictPattern = true,None)
    | "strict" ~> "pattern" ~> "at" ~> "level" ~> primParse.natural ^^ {(_,n) => ETPattern(isStrictPattern = true,Some(n))}
    | "closed" ~> "binder" ^^ ETBinder(isOpenBindersList = false)
    | "custom" ~> primParse._ident ~? at_level ~? constr_as_binder_kind ^^ {
      (_,i,n,b) => ETConstr[Option[ProductionLevel]](InCustomEntry(i),b,n)
    }
    )

  lazy val at_level: Parser[extend.ProductionLevel] = "at" ~> level

  lazy val constr_as_binder_kind: Parser[notationTerm.ConstrAsBinderKind] = (
    "as" ~> "ident" ^^ AsIdent
    | "as" ~> "pattern" ^^ AsIdentOrPattern
    | "as" ~> "strict" ~> "pattern" ^^ AsStrictPattern
  )

    lazy val level: Parser[extend.ProductionLevel] = (
      "level" ~> primParse.natural ^^ NumLevel
      | "next" ~> "level" ^^ NextLevel
    )

  /**
    * optional with outcome
    */
  lazy val opt_only_parsing: Parser[Option[CompatVersion]] = (
    "(" ~> "only" ~> "parsing" ~> ")" ^^ Some(Current)
    | "(" ~> "compat" ~> primParse.string <~ ")" ^^ {(_,s) => Some(helper.parseCompatVersion(s))}
  )

  object helper {
    def parseCompatVersion(s:String):CompatVersion = s match {
      case "8.10" => Current
      case "8.9" => V8_9
      case "8.8" => V8_8
      case _ => throw new IllegalArgumentException("Compatibility with version: " + s + " not supported")
    }
  }
}
