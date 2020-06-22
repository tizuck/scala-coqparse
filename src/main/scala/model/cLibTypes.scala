package model

object cLibTypes {
  object cSig {
    sealed trait Union [+A,+B]
    sealed case class Inl[A](a:A) extends Union[A,Nothing]
    sealed case class Inr[B](b:B) extends Union[Nothing,B]
  }

  object unicode {

    sealed trait NextUtf8Error
    case object EndOfInput extends NextUtf8Error
    /**
      * If [s] is some UTF-8 encoded string
      * and [i] is a position of some UTF-8 character within [s]
      * then [next_utf8 s i] returns [(j,n)] where:
      *    - [j] indicates the position of the next UTF-8 character
      *    - [n] represents the UTF-8 character at index [i]
      *
      * @param s utf-8 encoded string
      * @param i position of character to analyze in s
      * @return (position of next UTF-8 character,UTF-8 character at index [i]) or Error
      */
    def next_utf8(s:String,i:Int):Either[(Int,Char),NextUtf8Error]={
      val l = s.length - i
      if (l == 0) Right(EndOfInput) else {
        null
      }
    }

    /**
      * Trying to refutate the validation of an identifier
      *
      * @param s potential identifier
      * @return None, if no refutation, info about why s is no identifier
      */
    def ident_refutation(s:String):Option[(Boolean,String)] = {
      if (s == "..") None else {
        null
      }
    }
  }
}
