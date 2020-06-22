package redundancyCheck

import cats.Eval
import parseback._
import model.interpTypes.constrExpr.constructor.{App, ConstrExpr, Ref}
import parsebackParse.ParserFix._
import parseback.compat.cats._
import cats.Eval
import model.interpTypes.constrExpr.{ExplByName, Explication, InstanceExpr}
import model.{kernelTypes, libraryTypes}
import parsebackParse.termParse
import model.libraryTypes.libNames.Qualid
import parsebackParse.termParse.universe_level

object MinimizedGrammar extends scala.App {

  implicit val W = parseback.Whitespace(()|"""\s+""".r)

  lazy val app : Parser[ConstrExpr] = atomic_constr ~ appl_arg.++ ^^ {
    (_,c,ags) => {
      App((None,c),ags)
    }
  }

  lazy val appl_arg:Parser[(ConstrExpr,Option[Explication])]   = (
    "(" ~ ident ~ ":=" ~ atomic_constr ~ ")" ^^ { (_, _, i, _, c, _) => (c,Some(ExplByName(i)))}
      | atomic_constr ^^ {(_,c) => (c,None)}
    )

  lazy val atomic_constr : Parser[ConstrExpr] = termParse.global ~? instance  ^^ { (_,g,i) => i match{ case Some(x) => Ref(g,x) case None => Ref(g,None)}}

  lazy val instance:Parser[Option[InstanceExpr]] = (
    "@{" ~ universe_level.** ~ "}" ^^ { (_,_,l,_) => Some(l)}
    )

  lazy val global : Parser[Qualid] = reference

  lazy val reference: Parser[Qualid] = ident ^^ { (_, i) =>
    libraryTypes.libNames.qualid_of_ident(i)
  }

  lazy val ident: Parser[kernelTypes.id.T] = _ident ^^ { (_, i) => i }

  lazy val _ident: Parser[String] = _fsti  ^^ {(_,w) => w}


  val keywords : String = Seq("if","_","as","else","at","cofix","end","exists","exists2",
    "fix","for","forall","fun","if","IF","in","let","match","mod","Prop","return","Set",
    "then","Type","using","where","with","\\)","\\(","\\}","\\{","\\[","\\]").mkString("(",")|(",")")

  //TODO: Maybe find a better way for this hack
  lazy val _fsti: Parser[String] = s"""(?!(($keywords)\\s))\\w+""".r

  val input: LineStream[Eval] = LineStream[Eval]("eq_bool true true false")

  val result = app(input)

  if(result.value.isRight){
    //result.value.right.foreach(c => c.uncons.foreach(x => println(x._1)))
    result.value.right.foreach(c => c.toList.foreach(println))
    println(result.value)
  }
  else{
    println(result.value)
  }

}
