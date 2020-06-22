package parsebackParse

import parseback._
import parseback.compat.cats._
import ParserFix._
import implicits._
import cats.Eval
import model.interpTypes.constrExpr.constructor.{ConstrExpr, NotationEntry}
import model.vernacTypes.vernacExpr.Notation
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

object Main extends App {

  implicit val W : parseback.Whitespace = parseback.Whitespace("""\s+""".r.?)

  val input: LineStream[Eval] = LineStream[Eval]("Notation \"[ x ]\" := (cons x nil) : list_scope")
  val input2: LineStream[Eval] = LineStream[Eval]("Notation \"x -> y\" := (forall (_:x) , y) (at level 85, left associativity)")
  val generatedParserInput: LineStream[Eval] = LineStream[Eval]("a -> b")


  val result = syntaxParse.syntax(input)
  val result2 = syntaxParse.syntax(input2)

  if(result.value.isRight  && result2.value.isRight){
    /**
      * Add notation to parser interpretation
      */
    termParse.oprConstr.addNotation(result.value.right.get.toList.head.asInstanceOf[Notation])
    termParse.oprConstr.addNotation(result2.value.right.get.toList.head.asInstanceOf[Notation])
    println(termParse.oprConstr.reservedNotationExtensions)
    println(ParserHelper.printParser(termParse.oprConstr(200),30,0))
    val generatedParserResult = termParse.oprConstr(200)(generatedParserInput)
    if(generatedParserResult.value.isRight) {
      val term = generatedParserResult.value.right.get
      println("Amount of results: " + term.toList.size + ", with first result: " + term.toList.head)
      val substitutedNotation: ConstrExpr = term.toList.head match {
        case NotationEntry(_, rewritingStrategy) => rewrite(rewritingStrategy)(result2.value.right.get.toList.head.asInstanceOf[Notation].c)
      }
      println(substitutedNotation)
      if (term.toList.size > 1) {
        println("All results: ")
        term.toList.foreach(println)
      }
    } else {
      println(generatedParserResult.value)
    }
  }
  else{
    println(result.value)
  }



  /*
  import model.notationTypes._
  import model.metasyntax._

  val two = Some(200)
  println(compute_syntax_data("a -> b",List(SetAssoc(LeftA),SetLevel(74),SetItemLevel(List("b"),None,Some(NumLevel(200)))))(debugPrint = true))
   */

}