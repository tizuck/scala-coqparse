package model

import model.interpTypes.constrExpr.{ConstrPatternExpr, OrByNotation}
import model.interpTypes.constrExpr.constructor.ConstrExpr
import model.libTypes.util.Union
import model.libraryTypes.libNames.Qualid
import model.pretypingTypes.locus.WithOccurences
import model.pretypingTypes.typeclasses.HintInfoGen
import model.tacticsTypes.genRedExpr.{FDeltaBut, GlobRedFlag, RedAtom}

object tacticsTypes {

  object genRedExpr {

    type RawRedExpr = RedExprGen[ConstrExpr,OrByNotation[Qualid],ConstrExpr]

    sealed trait RedAtom [+A]
    case object FBeta extends RedAtom[Nothing]
    case object FMatch extends RedAtom[Nothing]
    case object FFix extends RedAtom[Nothing]
    case object FCofix extends RedAtom[Nothing]
    case object FZeta extends RedAtom[Nothing]
    sealed case class FConst[A](as:List[A]) extends RedAtom[A]
    sealed case class FDeltaBut[A](as:List[A]) extends RedAtom[A]

    sealed case class GlobRedFlag [A] (rBeta:Boolean,rMatch:Boolean,rFix:Boolean,rCofix:Boolean,
                                       rZeta:Boolean,rDelta:Boolean,rConst:List[A])

    sealed trait RedExprGen [+A,+B,+C]
    sealed case class Red(boolean: Boolean) extends RedExprGen[Nothing,Nothing,Nothing]
    case object Hnf extends RedExprGen[Nothing,Nothing,Nothing]
    sealed case class Simpl[B,C](globRedFlag: GlobRedFlag[B],wU:Option[WithOccurences[Union[B,C]]]) extends RedExprGen[Nothing,B,C]
    sealed case class Cbv[B](globRedFlag: GlobRedFlag[B]) extends RedExprGen[Nothing,B,Nothing]
    sealed case class Cbn[B](globRedFlag: GlobRedFlag[B]) extends RedExprGen[Nothing,B,Nothing]
    sealed case class Lazy[B](globRedFlag: GlobRedFlag[B]) extends RedExprGen[Nothing,B,Nothing]
    sealed case class Unfold[B](withOccurencess: List[WithOccurences[B]]) extends RedExprGen[Nothing,B,Nothing]
    sealed case class Fold[A](as:List[A]) extends RedExprGen[A,Nothing,Nothing]
    sealed case class Pattern[A](withOccurencess: List[WithOccurences[A]]) extends RedExprGen[A,Nothing,Nothing]
    sealed case class ExtraRedExpr(string: String) extends RedExprGen[Nothing,Nothing,Nothing]
    sealed case class CbvVm[B,C](wU:Option[WithOccurences[Union[B,C]]]) extends RedExprGen[Nothing,B,C]
    sealed case class CbvNative[B,C](wU:Option[WithOccurences[Union[B,C]]]) extends RedExprGen[Nothing,B,C]
  }

  object redOps {
    def makeRedFlag[A](redAtoms: List[RedAtom[A]]): GlobRedFlag[A] = {
      def addFlag(globRedFlag: GlobRedFlag[A]): List[RedAtom[A]] => GlobRedFlag[A] = {
        //TODO implement me
        _ => globRedFlag
      }
      addFlag(GlobRedFlag(rBeta = false,
        rMatch = false,
        rFix = false,
        rCofix = false,
        rZeta = false,
        rDelta = false,
        Nil))(redAtoms)
    }
  }

  object hints {
    type HintInfoExpr = HintInfoGen[ConstrPatternExpr]
    type bla = Int

  }
}
