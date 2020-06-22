package parsebackParse

import parseback.{Parser, Whitespace}
import ParserFix._

object ParserHelper {
  def list1WithSeperator[T,S](p:Parser[T],sep:Parser[S])(implicit b: Whitespace): Parser[List[T]] = {
    p ~? (sep ~> p).++ ^^ {
      (_,p,ps) => p :: ps.getOrElse(Nil)
    }
  }

  def list1WithSperatorAndOptParser[T,S](optP:Parser[T], sep:Parser[S])(implicit b: Whitespace):Parser[List[T]] = {
    (optP ~? (sep ~?> optP).++ ^^ { (_, p, ps) => p :: ps.getOrElse(Nil).flatten}
      | (sep ~?> optP).++ ^^ {(_,ps) => ps.flatten}
      )
  }

  //Todo: potentially epsilon
  def opt3[T,U,V](p1:Parser[T],p2:Parser[U],p3:Parser[V])(implicit b: Whitespace):Parser[(Option[T],Option[U],Option[V])] = {
    (
      p1 ^^ {(_,t) => (Some(t),None,None)}
      | p2 ^^ {(_,u) => (None,Some(u),None)}
      | p3 ^^ {(_,v) => (None,None,Some(v))}
      | p1 ~ p2 ^^ {(_,t,u) => (Some(t),Some(u),None)}
      | p1 ~ p3  ^^ {(_,t,v) => (Some(t),None,Some(v))}
      | p2 ~ p3 ^^ {(_,u,v) => (None,Some(u),Some(v))}
      | p1 ~ p2 ~ p3 ^^ {(_,t,u,v) => (Some(t),Some(u),Some(v))}
      )
  }

  //TODO: potentially epsilon
  def opt2[T,U](p1:Parser[T],p2:Parser[U])(implicit b: Whitespace):Parser[(Option[T],Option[U])] = {
    (
      p1 ^^ {(_,t) => (Some(t),None)}
      | p2 ^^ {(_,u) => (None,Some(u))}
      | p1 ~ p2 ^^ {(_,t,u) => (Some(t),Some(u))}
      )
  }

  def printParser[T](p:Parser[T],depth:Int,indentation:Int):String = {
    val indent = Seq.iterate(" ",indentation)(x => x).mkString("")
    if (depth > 0) {
      p match {
        case Parser.Sequence(left, layout, right) => s"~\n $indent ${printParser(left,depth-1,indentation + 1)} \n $indent ${printParser(right,depth-1,indentation + 1)} "
        case Parser.Union(_left, _right) => s"|\n $indent ${printParser(_left(),depth-1,indentation+1)} \n $indent  ${printParser(_right(),depth-1,indentation +1)}"
        case Parser.Apply(target, f, lines) => s"[apply:\n $indent ${printParser(target,depth-1,indentation +1)} \n $indent , f: ${f.toString()} , lines: ${lines.toString()}]"
        case Parser.Filter(target, p) => s"[filt: \n $indent ${printParser(target,depth-1,indentation +1)} \n $indent , ${p.toString()} ]"
        case Parser.Literal(literal, offset) => literal
        case Parser.Regex(r) => r.toString()
        case Parser.Epsilon(value) => "eps"
        case Parser.Failure(errors) => "fail"
      }
    } else {
      ".."
    }
  }
}
