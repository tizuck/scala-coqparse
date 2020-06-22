package parsebackParse

import model.{interpTypes, kernelTypes, libraryTypes}
import parseback._
import ParserFix._
import model.libraryTypes.libNames.Qualid
import parsebackParse.ParserFix._

//TODO:Many types like pattern_identref do not actually just return an Ident, but an Ident with a AST information found in CAst.T, discuss necessity

object primParse {

  implicit val W : parseback.Whitespace = parseback.Whitespace("""\s+""".r.?)

  lazy val ident: Parser[kernelTypes.id.T] = _ident ^^ { (_, i) => i }

  lazy val pattern_ident: Parser[kernelTypes.id.T] = "?" ~ _ident ^^ { (_, _, i) => i }

  //TODO:This is actually a CAst.T with generic type ident, discuss need for this
  lazy val pattern_identref: Parser[kernelTypes.id.T] = pattern_ident ^^ { (_, i) => i }

  lazy val vari: Parser[kernelTypes.id.T] = ident ^^ { (_, i) => i }

  lazy val identref: Parser[kernelTypes.id.T] = ident ^^ { (_, i) => i }

  lazy val field: Parser[kernelTypes.id.T] = "." ~ ident ^^ { (_,_,f) => f }

  lazy val fields: Parser[(List[kernelTypes.id.T], kernelTypes.id.T)] = field ~ fields ^^ {
    (_: List[Line], id: kernelTypes.id.T, l: List[kernelTypes.id.T], i: kernelTypes.id.T) => (l ++ List(id), i)
  } |
    field ^^ { (_, id) => (Nil, id) }

  lazy val fullyqualid: Parser[List[kernelTypes.id.T]] = ident ~ fields ^^ { (_: List[Line], id: kernelTypes.id.T, l: List[kernelTypes.id.T], i: kernelTypes.id.T) => id :: (i :: l).reverse } |
    ident ^^ { (_, i) => List(i) }

  lazy val basequalid: Parser[Qualid] = ident ~ fields ^^ { (_: List[Line], i: kernelTypes.id.T, l: List[kernelTypes.id.T], i2: kernelTypes.id.T) => libraryTypes.libNames.make_qualid(l ++ List(i))(i2) } |
    ident ^^ { (_, i) => libraryTypes.libNames.qualid_of_ident(i) }

  lazy val name: Parser[kernelTypes.name.T] = (
    "_" ^^ { (_, _) => kernelTypes.name.Anonymous }
      | ident ^^ { (_, id) => kernelTypes.name.Name(id) }
    )

  lazy val reference: Parser[Qualid] = ident ~ fields ^^ { (_: List[Line], i: kernelTypes.id.T, l: List[kernelTypes.id.T], i2: kernelTypes.id.T) =>
    libraryTypes.libNames.make_qualid(l ++ List(i))(i2)
  } |
    ident ^^ { (_, i) =>
    libraryTypes.libNames.make_qualid(Nil)(i)
  }

  lazy val by_notation: Parser[(String, Option[String])] = ne_string ~? ("%" ~> _ident) ^^ { (_, neS, oI) => (neS, oI) }

  lazy val smart_global: Parser[interpTypes.constrExpr.OrByNotation[Qualid]] = reference ^^ { (_, r) => interpTypes.constrExpr.AN(r) } |
    by_notation ^^ { (_: List[Line], s: String, oI: Option[kernelTypes.id.T]) => interpTypes.constrExpr.ByNotation(s, oI) }

  lazy val qualid: Parser[Qualid] = basequalid

  lazy val ne_string: Parser[String] = string ^^ { (_, s) => if (s.equals("")) throw new IllegalStateException() else s }

  //TODO: Add lexical definition of String
  lazy val string: Parser[String] = "\"" ~> string_body <~ "\"" ^^ {(_,s) => s}

  lazy val string_body: Parser[String] = ("[^\"]+".r|"") ^^ { (_, s) => s}

  lazy val ne_lstrubg: Parser[String] = ne_string

  lazy val dirpath: Parser[kernelTypes.dirpath.T] = ident ~? field.++ ^^ { (_, i, fs) => kernelTypes.dirpath.make((i :: fs.getOrElse(Nil)).reverse) }

  lazy val lstring: Parser[String] = ne_string

  lazy val integer: Parser[Int] = int ^^ { (_, i) => i } |
    "-" ~ int ^^ { (_, _, i) => -i }

  lazy val int: Parser[Int] = """\d+""".r ^^ { (_,n) =>  n.toInt}

  lazy val natural: Parser[Int] = int

  lazy val bigint: Parser[Int] = int

  lazy val _ident: Parser[String] = _fsti  ^^ {(_,w) => w}

  val keywords : String = Seq("if","_","as","else","at","cofix","end","exists","exists2",
  "fix","for","forall","fun","if","IF","in","let","match","mod","Prop","return","Set",
  "then","Type","using","where","with","\\)","\\(","\\}","\\{","\\[","\\]").mkString("(",")|(",")")

  //TODO: Maybe find a better way for this hack
  lazy val _fsti: Parser[String] = s"""(?!(($keywords|\\d)\\s))\\w+""".r
}

