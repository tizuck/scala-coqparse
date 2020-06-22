package parsebackParse.ltac

import model.cLibTypes.cSig.{Inl, Inr}
import model.interpTypes.constrExpr.OrByNotation
import model.interpTypes.constrExpr.constructor.ConstrExpr
import model.libTypes.util.Union
import model.libraryTypes.libNames.Qualid
import model.pretypingTypes.locus._
import model.tacticsTypes.genRedExpr._
import parseback._
import parsebackParse.ParserFix._

object tacticParse {

  lazy val int_or_var : Parser[OrVar[Int]]= (
    parsebackParse.primParse.integer ^^ {(_,n) => ArgArg(n)}
    | parsebackParse.primParse.identref ^^ {(_,id) => ArgVar(id)}
  )
  lazy val nat_or_var : Parser[OrVar[Int]] = (
    parsebackParse.primParse.natural ^^ {(_,n) => ArgArg(n)}
    | parsebackParse.primParse.identref ^^ {(_,id) => ArgVar(id)}
  )

  lazy val red_flags : Parser[List[RedAtom[OrByNotation[Qualid]]]] = (
    "beta" ^^ {(_,_) => List(FBeta)}
    | "iota" ^^ {(_,_) => FMatch :: FFix :: FCofix :: Nil}
    | "match" ^^ {(_,_) => List(FMatch)}
    | "fix" ^^ {(_,_) => List(FFix)}
    | "cofix" ^^ {(_,_) => List(FCofix)}
    | "zeta" ^^ {(_,_) => List(FZeta)}
    | "delta" ~ delta_flag ^^ {(_,_,d) => List(d)}
  )
  //TODO: Potential epsilon with FDeltaBut(Nil) as result
  lazy val delta_flag : Parser[RedAtom[OrByNotation[Qualid]]] = (
    "-" ~ "[" ~ parsebackParse.primParse.smart_global.++ ~ "]" ^^ {(_,_,_,sGs,_) => FDeltaBut(sGs)}
    | "[" ~ parsebackParse.primParse.smart_global.++ ~ "]" ^^ {(_,_,sGs,_) => FConst(sGs)}
  )

  lazy val strategy_flag: Parser[GlobRedFlag[OrByNotation[Qualid]]] = (
    red_flags.++ ^^ {(_,rfs) => model.tacticsTypes.redOps.makeRedFlag(rfs.flatten)}
    | delta_flag ^^ {(_,d) => functions.all_with(d)}
  )

  lazy val red_expr: Parser[RedExprGen[ConstrExpr, OrByNotation[Qualid], ConstrExpr]] = (
    "red" ^^ {(_,_) => Red(false)}
    | "hnf" ^^ {(_,_) => Hnf}
    | helper.simplHelper ~? delta_flag ~? ref_or_pattern_occ ^^ {(_,_,df,rps) =>
      Simpl(functions.all_with(df.getOrElse(FDeltaBut(Nil))),rps)}
    | "cbv" ~ strategy_flag ^^ {(_,_,sf) => Cbv(sf)}
    | "cbn" ~ strategy_flag ^^ {(_,_,sf) => Cbn(sf)}
    | "lazy" ~ strategy_flag ^^ {(_,_,sf) => Lazy(sf)}
    | "compute" ~ delta_flag ^^ {(_,_,df) => Cbv(functions.all_with(df))}
    | helper.vm_compute_helper ~? ref_or_pattern_occ ^^ {(_,_,rf) => CbvVm(rf)}
    | helper.native_compute_helper ~? ref_or_pattern_occ ^^ {(_,_,rf) => CbvNative(rf)}
    | "unfold" ~ unfold_occ ~? ("," ~ unfold_occ).++ ^^ {
      (_:List[Line],
        _:String,
        occl:OccurrencesGen[OrVar[Int]],
        occr:OrByNotation[Qualid],
        occUnfold:Option[List[(String,(OccurrencesGen[OrVar[Int]],OrByNotation[Qualid]))]]) =>
          Unfold((occl,occr) :: occUnfold.getOrElse(Nil).map(_._2))
    }
    | "fold" ~ parsebackParse.termParse.constr.++ ^^ {(_,_,cs) => Fold(cs)}
    | "pattern" ~ pattern_occ ~? ("," ~ pattern_occ).++ ^^ {
      (_:List[Line],_:String,poccl:OccurrencesGen[OrVar[Int]],poccr:ConstrExpr,
       poccs:Option[List[(String,(OccurrencesGen[OrVar[Int]],ConstrExpr))]]) =>
        Pattern((poccl,poccr) :: poccs.getOrElse(Nil).map(_._2))
    }
    | parsebackParse.primParse.ident ^^ {(_,id) => ExtraRedExpr(id) }
  )

  lazy val occs_nums : Parser[OccurrencesGen[OrVar[Int]]] = (
    nat_or_var.++ ^^ {(_,ns) => OnlyOccurrences(ns)}
    | "-" ~ nat_or_var ~? int_or_var.++ ^^ {(_,_,n,ns) =>
      val names = n::ns.getOrElse(Nil)
      val mappedNames = names.map(functions.map_int_or_var(scala.math.abs))
      AllOccurrencesBut(mappedNames)}
  )
  //TODO: Potentially epsilon with AllOccurences as result
  lazy val opt_occs : Parser[OccurrencesGen[OrVar[Int]]] = "at" ~ occs_nums ^^ { (_, _, occs) => occs}

  lazy val ref_or_pattern_occ:Parser[(OccurrencesGen[OrVar[Int]],Union[OrByNotation[Qualid],ConstrExpr])] = (
    parsebackParse.primParse.smart_global ~? opt_occs ^^ { (_, g, occs) => (occs.getOrElse(AllOccurrences),Inl(g))}
    | parsebackParse.termParse.constr ~? opt_occs ^^ { (_, c, occs) => (occs.getOrElse(AllOccurrences),Inr(c))}
  )

  lazy val pattern_occ: Parser[(OccurrencesGen[OrVar[Int]], ConstrExpr)] = parsebackParse.termParse.constr ~? opt_occs ^^ { (_, c, occs) =>
    (occs.getOrElse(AllOccurrences),c)
  }

  lazy val unfold_occ: Parser[(OccurrencesGen[OrVar[Int]], OrByNotation[Qualid])] = parsebackParse.primParse.smart_global ~? opt_occs ^^ { (_, g, occs) =>
    (occs.getOrElse(AllOccurrences),g)}

  object helper {
    lazy val simplHelper : Parser[String] = "simpl" ^^ {(_,s) => s}
    lazy val vm_compute_helper : Parser[String] = "vm_compute" ^^ {(_,s) => s}
    lazy val native_compute_helper : Parser[String] = "native_compute" ^^ {(_,s) => s}
  }

  object functions {
    def map_int_or_var[A,B](f:A => B): OrVar[A] => OrVar[B] = {
      case ArgArg(v) => ArgArg(f(v))
      case ArgVar(id) => ArgVar(id)
    }

    def all_with[A] (redAtom:RedAtom[A]): GlobRedFlag[A] = model.tacticsTypes.redOps.makeRedFlag(
      FBeta ::
      FMatch ::
      FFix ::
      FCofix ::
      FZeta ::
        redAtom ::
        Nil)
  }
}
