package parsebackParse

import parseback._

import scala.language.implicitConversions

object ParserFix {
  trait EnhancedParser [+A] {
    def ??()(implicit b: Whitespace): Parser[Option[A]]

    def ~?[B](that:Parser[B])(implicit b : Whitespace) : Parser[(A,Option[B])]

    def ~?>[B](that:Parser[B])(implicit b : Whitespace) : Parser[Option[B]]

    def ?~[B](that:Parser[B])(implicit b : Whitespace) : Parser[(Option[A],B)]

    def **()(implicit b: Whitespace): Parser[List[A]]

    def ++()(implicit b: Whitespace): Parser[List[A]]
  }
  implicit def toEnhancedParser[A](p:Parser[A]):EnhancedParser[A] = new EnhancedParser[A] {
    override def ??()(implicit b: Whitespace): Parser[Option[A]] = {
      "" ^^ {(_,_) => None} | (p map { Some(_) })
    }

    override def ~?[B](that: Parser[B])(implicit b: Whitespace): Parser[(A, Option[B])] = {
      p ~ that ^^ {(_,x,y) => (x,Some(y))}|
        p ^^ {(_,x) => (x,None)}
    }

    override def ~?>[B](that: Parser[B])(implicit b: Whitespace): Parser[Option[B]] = {
      p ~ that ^^ {(_,_,y) => Some(y)}
      p ^^ {(_,_) => None}
    }

    override def ?~[B](that: Parser[B])(implicit b: Whitespace): Parser[(Option[A],B)] = {
      p ~ that  ^^ {(_,x,y) => (Some(x),y)}|
        that ^^ {(_,x) => (None,x)}
    }

    override def **()(implicit b: Whitespace): Parser[List[A]] = {
      lazy val back: Parser[List[A]] = (
        p ~ back   ^^ { (_, h, t) => h :: t }
          | ""           ^^^ Nil
        )
      back
    }

    override def ++()(implicit b: Whitespace): Parser[List[A]] = {
      lazy val back: Parser[List[A]] = (
        p ~ back   ^^ { (_, h, t) => h :: t }
          | p          ^^ { (_, h) => h :: Nil }
        )
      back
    }
  }
}
