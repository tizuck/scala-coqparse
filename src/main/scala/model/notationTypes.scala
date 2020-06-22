package model

import cats.Eval
import model.interpTypes.constrExpr.{NotationEntryLevel, NotationKey}
import parseback._
import parsebackParse.ParserFix._
import parseback.compat.cats._


/**
  * Wrapper object for types and functions defined in [[https://github.com/coq/coq/blob/a37b34723778679203af09f5f63476ea2204ceb0/interp/notation.ml]]
  *
  */
object notationTypes {

  sealed trait Symbol
  sealed case class Terminal(s:String) extends Symbol
  sealed case class NonTerminal(id:kernelTypes.id.T) extends Symbol
  sealed case class SProdList(id:kernelTypes.id.T,symbols:List[Symbol]) extends Symbol
  sealed case class Break(n:Int) extends Symbol

  sealed trait SymbolToken
  sealed case class WhiteSpace(i:Int) extends SymbolToken
  sealed case class SymbolTokenString(s:String) extends SymbolToken

  def decompose_raw_notation(ntn:String):List[Symbol] = raw_analyze_notation_tokens(split_notation_string(ntn))
  /**
    * Parser for a list of tokens resulting from using [[split_notation_string]].
    *
    * @param l list of SymbolTokens to be parsed
    * @return list of parsed Symbols
    */
  def raw_analyze_notation_tokens(l:List[SymbolToken]):List[Symbol] = l match {
    case Nil => Nil
    case SymbolTokenString("..") :: sl => NonTerminal("..") :: raw_analyze_notation_tokens(sl)
    case SymbolTokenString("_") :: _ => throw new IllegalArgumentException("_ must be quoted")
    case SymbolTokenString(x) :: sl  =>
      val input: LineStream[Eval] = LineStream[Eval](x)
      val result = parser.ident(input)
      //If parsing succeeds a valid identifier has been found
      if(result.value.isRight) NonTerminal(x) :: raw_analyze_notation_tokens(sl)
      else Terminal(x) :: raw_analyze_notation_tokens(sl)
    case WhiteSpace(n) :: sl => Break(n) :: raw_analyze_notation_tokens(sl)
  }
  /**
    * Lexer for a notation string.
    *
    * @param str notation string
    * @return list of Tokens
    */
  def split_notation_string(str:String):List[SymbolToken] = {

    def push_token(beg:Int,i:Int,l:List[SymbolToken]):List[SymbolToken] = {
      if (beg == i) l else SymbolTokenString(str.substring(beg,beg + (i - beg))) :: l
    }

    def push_whitespace(beg:Int,i:Int,l:List[SymbolToken]) : List[SymbolToken] = {
      if(beg == i) l else WhiteSpace(i-beg) :: l
    }

    @scala.annotation.tailrec
    def loop(beg:Int, i:Int):List[SymbolToken] = {
      if(i < str.length){
        if( str.charAt(i) == ' ') push_token(beg,i,loop_on_whitespace(i+1,i+1)) else loop(beg,i+1)
      } else {
        push_token(beg,i,Nil)
      }
    }

    @scala.annotation.tailrec
    def loop_on_whitespace(beg: Int, i: Int):List[SymbolToken] = {
      if( i < str.length){
        if(str.charAt(i) != ' ') push_whitespace(beg,i,loop(i,i+1)) else loop_on_whitespace(beg,i+1)
      } else {
        push_whitespace(beg,i,Nil)
      }
    }

    loop(0,0)
  }

  def string_of_symbol(symbol: Symbol):List[String] = symbol match {
    case NonTerminal(_) => List("_")
    case Terminal("_") => List("'_'")
    case Terminal(s) => List(s)
    case SProdList(_, symbols) =>
      val newL = symbols.flatMap(string_of_symbol)
      ("_" :: newL) ++ (".." :: newL) ++ List("_")
    case Break(n) => Nil
  }

  def make_notation_key(from:NotationEntryLevel, symbols:List[Symbol]): (NotationEntryLevel, String) =
    (
      from,
      symbols.flatMap(string_of_symbol).foldLeft(""){(acc,s) => acc + " " + s}.replaceFirst(" ","")
    )

  /**
    * Implementation for check of identifiers oriented towards:
    * [[https://coq.inria.fr/distrib/current/refman/language/gallina-specification-language.html]]
    *
    * @todo implement UTF-8 support
    */
  object parser {
    lazy val first_letter: Parser[String] = {
      "[a-zA-Z_]".r
    }
    lazy val subsequent_letter: Parser[String] = {
      "[a-zA-Z0-9_']".r
    }
    lazy val ident: Parser[String] = first_letter ~? subsequent_letter.++ ^^ { (_, f, subs) => f + subs.getOrElse(Nil).foldLeft("") { (acc, s) => acc + s } }
  }
}
