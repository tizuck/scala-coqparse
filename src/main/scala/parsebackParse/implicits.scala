package parsebackParse

import parseback.{Line, ~}

import scala.language.implicitConversions

//oriented towards: https://bitbucket.org/inkytonik/kiama/src/6a3bf9156a6007b75effaea45159549cbfae3642/wiki/ParserCombs.md
object implicits {

  implicit def constToTupleFunction1[A,R](r:A => R):(List[Line],A) => R = {
    (l,a) => r (a)
  }
  implicit def constToTupleFunction2[A,B,R](r: (A,B) => R) : (List[Line],A,B) => R = {
    (l,a,b) => r (a,b)
  }

  implicit def constToTupleFunction3[A,B,C,R](r:(A,B,C) => R) : (List[Line],A,B,C) => R = {
    (l,a,b,c) => r (a,b,c)
  }

  implicit def constToTupleFunction0[A,R](r: R) : (List[Line],A) => R = ((l,a) => r)
}
