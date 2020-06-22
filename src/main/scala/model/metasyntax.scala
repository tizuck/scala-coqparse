package model
import model.gramlibTypes.gramext.{GAssoc, NonA}
import model.interpTypes.constrExpr
import model.interpTypes.constrExpr.{InConstrEntry, InConstrEntrySomeLevel, InCustomEntry, InCustomEntryLevel, Notation, NotationEntry, NotationKey}
import model.interpTypes.notationTerm.{ConstrAsBinderKind, ConstrEntryKeyGen, ETBigint, ETBinder, ETConstr, ETGlobal, ETIdent, ETPattern, NotationVarInternalizationType}
import model.kernelTypes.convOracle.Level
import model.kernelTypes.id.T
import model.libTypes.flags.CompatVersion
import model.parsingTypes.extend.{BorderProd, ConstrEntryKey, InternalProd, NumLevel, ProductionLevel, ProductionPosition, SimpleConstrProdEntryKey}
import model.vernacTypes.vernacExpr.{SetAssoc, SetCompatVersion, SetCustomEntry, SetEntryType, SetFormat, SetItemLevel, SetLevel, SetOnlyParsing, SetOnlyPrinting, SyntaxModifier}
import notationTypes._

object metasyntax {

  sealed case class NotationModifier(assoc:Option[GAssoc] = None,
                                     level : Option[Int] = None,
                                     custom : NotationEntry = InConstrEntry,
                                     eTypes : List[(kernelTypes.id.T,SimpleConstrProdEntryKey)] = Nil,
                                     subTypes: List[(kernelTypes.id.T,ProductionLevel)] = Nil,
                                     only_parsing : Boolean = false,
                                     only_printing : Boolean = false,
                                     compat : Option[CompatVersion] = None,
                                     format : Option[String] = None,
                                     extra : List[(String,String)] = Nil)

  def get_notation_vars(l:List[Symbol],onlyPrint:Boolean):List[String] = l match {
    case Nil => Nil
    case NonTerminal(id) :: sl =>
      val vars = get_notation_vars(sl,onlyPrint)
      if (id == "..") vars
      else
        if (!onlyPrint && vars.contains(id)) throw new IllegalStateException("Identifier: " + id + " used multiple times")
        else
          id :: vars
    case Terminal(_) :: sl => get_notation_vars(sl,onlyPrint)
    case Break(_) :: sl => get_notation_vars(sl,onlyPrint)
    case SProdList(_,_) :: _ =>throw new IllegalStateException("assert false")
  }

  def analyze_notation_tokens(ntn:String,onlyPrint:Boolean)(implicit debug:Boolean = false): (List[(String, String)], List[String], List[Symbol]) = {
    val l = decompose_raw_notation(ntn)
    if(debug)println("Decomposed notation: " + l)
    val vars = get_notation_vars(l,onlyPrint)
    //TODO: Not sure if necessary
    if(debug)println("Variables of the notation: " + vars)
    val interp = interp_list_parser(Nil)(l)
    if(debug)println("List parser: " + interp)
    (interp._1,vars.diff(interp._1.map(_._2)),l)
  }

  def out_nt:Symbol => String = {case NonTerminal(id) => id case _ => throw new IllegalStateException("assert false")}
  def find_pattern(nt:Symbol,xl:List[Symbol]):(List[Symbol],List[Symbol]) => ((String,String,List[Symbol]),List[Symbol]) = {
    case ((x@Break(n)) :: l,Break(n2)::l2) if n == n2 => find_pattern(nt,x::xl)(l,l2)
    case ((x@Terminal(s)) :: l,Terminal(s2)::l2) if s equals s2 => find_pattern(nt,x::xl)(l,l2)
    case (Nil,NonTerminal(id)::l) =>((out_nt(nt),id,xl.reverse),l)
    case _ => throw new IllegalStateException("Fix me!!")
  }

  /**
    * @todo implement recursive-form-suggestion in Exception, find out what this method does
    * @param hd
    * @return
    */
  def interp_list_parser(hd:List[Symbol]): List[Symbol] => (List[(String,String)],List[Symbol]) = {
    case Nil => (Nil,hd.reverse)
    case NonTerminal(id) :: tl if id equals ".." =>
      if (hd.isEmpty) throw new IllegalStateException("Different recursive form expected")
      val newHd = hd.reverse
      val pattern = find_pattern (newHd.head,Nil)(newHd.tail,tl)
      val interp = interp_list_parser(Nil)(pattern._2)
      (((pattern._1._1,pattern._1._2) :: interp._1),SProdList(pattern._1._1,pattern._1._3) :: interp._2)
    case (s@(Terminal(_)|Break(_))) :: tl =>
      if(hd.isEmpty){
        val interp = interp_list_parser(Nil)(tl)
        (interp._1,s :: interp._2)
      } else {
        interp_list_parser(s::hd)(tl)
      }
    case (x@NonTerminal(_)) :: tl =>
      val interp = interp_list_parser(List(x))(tl)
      (interp._1,hd.reverse ++ interp._2)
  }

  //TODO:Write proper Exceptions
  def interp_modifiers(mods:List[SyntaxModifier]):NotationModifier = {
    @scala.annotation.tailrec
    def interp(subTypes:List[(T,Option[ConstrAsBinderKind], Option[ProductionLevel])], acc: NotationModifier, mods:List[SyntaxModifier]):
    (List[(T,Option[ConstrAsBinderKind], Option[ProductionLevel])],NotationModifier) = mods match {
      case Nil => (subTypes,acc)
      case SetEntryType(s,typ) :: l =>
        if (acc.eTypes.count(t => t._1 == s) != 0) throw new IllegalStateException(s + "is allready assigned to an entry or constr level")
        interp(subTypes,acc.copy(eTypes = (s,typ) :: acc.eTypes),l)
      case SetItemLevel(Nil,bko,n) :: l =>interp(subTypes,acc,l)
      case SetItemLevel(s::idl,bko,n) :: l =>
        if (acc.eTypes.count(t => t._1 == s) != 0) throw new IllegalStateException(s + "is allready assigned to an entry or constr level")
        interp((s,bko,n)::subTypes,acc,SetItemLevel(idl,bko,n)::l)
      case SetLevel(n) :: l => acc.custom match {
        case InCustomEntry(s) =>
          if (acc.level.isDefined) throw new IllegalStateException("isolated at level " + n + " unexpected")
          else
            throw new IllegalStateException("use in custom" + s + " at level " + n + "rather than" + " at level" + n + " isolated.")
        case InConstrEntry =>
          if (acc.level.isDefined) throw new IllegalStateException("A level is allready assigned")
          interp(subTypes,acc.copy(level=Some(n)),l)
      }
      case SetCustomEntry(s,n) :: l =>
        if (acc.level.isDefined)
          if(n.isEmpty) throw new IllegalStateException("use \" in custom " + s + " at level " + acc.level.get +
            "\"" + "rather than" + "\"at level " + acc.level.get + "\"" + "isolated.")
          else
            throw new IllegalStateException("isolated \"at level" + acc.level.get + "\" unexpected.")
        if (acc.custom != InConstrEntry)
          throw new IllegalStateException("Entry is allready assigned to custom " + s)
        interp(subTypes,acc.copy(custom = InCustomEntry(s), level=n),l)
      case SetAssoc(a) :: l =>
        if (acc.assoc.isDefined) throw new IllegalStateException("An associativity is given more than once.")
        interp(subTypes,acc.copy(assoc = Some(a)),l)
      case SetOnlyParsing :: l =>
        interp(subTypes,acc.copy(only_parsing = true),l)
      case SetOnlyPrinting :: l =>
        interp(subTypes,acc.copy(only_printing = true),l)
      case SetCompatVersion(v) :: l =>
        interp(subTypes,acc.copy(compat = Some(v)),l)
      case SetFormat("text",s) :: l =>
        if (acc.format.isDefined) throw new IllegalStateException("A format is given more than once.")
        interp(subTypes,acc.copy(format = Some(s)),l)
      case SetFormat(k,s) :: l =>
        interp(subTypes,acc.copy(extra = (k,s)::acc.extra),l)
    }
    val result = interp(Nil,NotationModifier(),mods)
    val extra_etyps = result._1.map{trip => (trip._1,ETConstr(result._2.custom,trip._2,trip._3))}
    result._2.copy(eTypes = (extra_etyps ++ result._2.eTypes))
  }

  def assoc[A,B](l:List[(A,B)],key:A):Option[B] = {
    val entry = l.find(pair => pair._1 equals key)
    if (entry.isDefined)Some(entry.get._2) else None
  }

  /**
    *
    * @param custom
    * @param level
    * @param etyps
    * @param symbols
    * @param onlyPrinting
    * @return tuple consisting of feedback message and precedence level
    */
  def find_precedence(custom:NotationEntry,
                      level:Option[Int],
                      etyps:List[(kernelTypes.id.T,
                        SimpleConstrProdEntryKey)],
                      symbols:List[Symbol],
                      onlyPrinting:Boolean): (List[String], Int) = {
    def first_symbol: Option[Symbol] = {
      def faux:List[Symbol] => Option[Symbol]  = {
        case Break(_) :: t => faux(t)
        case h :: t => Some(h)
        case Nil => None
      }
      faux(symbols)
    }
    def last_is_terminal: Boolean = {
      def aux:Boolean => List[Symbol] => Boolean = isTermnial => {
        case Break(_) :: t => aux(isTermnial)(t)
        case Terminal(_) :: t => aux(true)(t)
        case _ :: t => aux(false)(t)
        case Nil => isTermnial
      }
      aux(false)(symbols)
    }
    first_symbol match {
      case None => (Nil,0)
      case Some(NonTerminal(id)) =>
        def test():(List[String],Int) = {
          if (onlyPrinting) if (level.isEmpty) throw new IllegalStateException() else (Nil, level.get)
          else throw new IllegalStateException()
        }
        (assoc(etyps,id),custom) match {
          case (Some(ETConstr(s,_,Some(_))),s2) if s equals s2 => test()
          case (Some(ETIdent),_)|(Some(ETBigint),_)|(Some(ETGlobal),_) =>
            level match {
              case None => (List("Setting notation at level 0"),0)
              case Some(0) => (Nil,0)
              case _ => throw new IllegalArgumentException("A notation starting with an atomic expression must be at level 0")
            }
          case (Some(ETPattern(_,_)),_)| (Some(ETBinder(_)),_) | (Some(ETConstr(_,_,_)),_) =>
            if (level.isEmpty) throw new IllegalStateException("Need an explicit level") else (Nil,level.get)
          case (None,_) => if (level.isEmpty)
            throw new IllegalStateException("A left recursive notation must have an explicit level") else (Nil,level.get)
        }
      case Some(Terminal(s)) if last_is_terminal =>
        if (level.isEmpty) (List("Setting notation level to 0."),0)
        else (Nil,level.get)
      case Some(_) =>
        if(level.isEmpty) throw new IllegalStateException("Can not determine the level.")
        else (Nil,level.get)
    }

  }

  def make_custom_entry(custom:NotationEntry,level:Int): constrExpr.NotationEntryLevel = {
    custom match {
      case InConstrEntry => InConstrEntrySomeLevel
      case InCustomEntry(s) => InCustomEntryLevel(s,level)
    }
  }

  /**
    *
    * @param symbols
    * @return
    * @todo treat special curly_brackets case
    */
  def remove_curly_brackets(symbols: List[notationTypes.Symbol]): List[Symbol] = {
    @scala.annotation.tailrec
    def skip_break(acc:List[notationTypes.Symbol])(syms:List[Symbol]) :(List[Symbol],List[Symbol]) = syms match {
      case (br@Break(_)) :: l => skip_break(br::acc)(l)
      case l => (acc.reverse, l)
    }

    def aux:Boolean => List[Symbol] => List[Symbol] = deb => {
      case Nil => Nil
      case (t1@Terminal("{")) :: l =>
        val pair = skip_break(Nil)(l)
        pair._2 match {
          case (x@NonTerminal(_)) :: l2 =>
            val pair2 = skip_break(Nil)(l2)
            pair2._2 match {
              case (t2@Terminal("}")) :: l3 =>
                if (deb && l3.isEmpty) List(t1,x,t2)
                else {
                  x :: aux(false)(l3)
                }
              case l1 => (t1 :: pair._1) ++ (x :: pair2._1) ++ aux(false)(l1)
            }
          case l0 => t1 :: aux(false)(l0)
        }
      case x :: l => x :: aux(false)(l)
    }
    aux(true)(symbols)
  }

  def check_rule_productivity(symbols: List[notationTypes.Symbol]):Unit = {
    if(symbols.forall { case Terminal(s) => false
    case NonTerminal(id) => true
    case SProdList(id, symbols) => false
    case Break(n) => true
    }) throw new IllegalStateException("Notation must include at least one symbol.")
    if(symbols match { case SProdList(_,_) :: l => true case _ => false})
      throw new IllegalStateException("A recursive notation must start with at least one symbol")
  }

  def join_auxiliary_recursive_types(recvars:List[(String,String)],etypes:List[(kernelTypes.id.T,
    SimpleConstrProdEntryKey)]): List[(T, SimpleConstrProdEntryKey)] = {
    recvars.foldRight(etypes){
      (pair,acc) =>
        val xtyp = assoc(etypes,pair._1)
        val ytyp = assoc(etypes,pair._2)
        (xtyp,ytyp) match {
          case(None,None) => acc
          case (Some(_),None) => acc
          case (None,Some(value)) => (pair._1,value) :: acc
          case (Some(xtyp),Some(ytyp)) if xtyp equals ytyp => acc
          case _ => throw new IllegalStateException()
        }
    }
  }

  def find_symbols(c_current: (parsingTypes.extend.NumLevel, parsingTypes.extend.ProductionPosition),
                   c_next: (parsingTypes.extend.NumLevel, parsingTypes.extend.ProductionPosition),
                   c_last: (parsingTypes.extend.NumLevel, parsingTypes.extend.ProductionPosition)):
  List[Symbol] => List[(String,(NumLevel,ProductionPosition))] = {
    case Nil => Nil
    case NonTerminal(id) :: sl =>
      val prec = if(sl.nonEmpty) c_current else c_last
      (id,prec) :: find_symbols(c_next,c_next,c_last)(sl)
    case Terminal(s) :: sl => find_symbols(c_next,c_next,c_last)(sl)
    case Break(n) :: sl => find_symbols(c_current,c_next,c_last)(sl)
    case SProdList(x,_) :: sl => (x,c_next) :: (find_symbols(c_next,c_next,c_last)(sl))
  }

  def set_entry_type(entry: interpTypes.constrExpr.NotationEntry, etyps:List[(kernelTypes.id.T,SimpleConstrProdEntryKey)]):
  (String,(NumLevel,ProductionPosition)) => (String,ConstrEntryKeyGen[(ProductionLevel,ProductionPosition)]) = (x,typ) => {
    val resultTyp:ConstrEntryKeyGen[(ProductionLevel,ProductionPosition)]= (assoc(etyps,x),typ) match {
      case (None,_) => ETConstr(entry,None,typ)
      case (Some(ETConstr(s,bko,Some(n))),(_,BorderProd(parsingTypes.extend.Left,_))) =>
        ETConstr(s,bko,(n,BorderProd(parsingTypes.extend.Left,None)))
      case (Some(ETConstr(s,bko,Some(n))),(_,InternalProd)) =>
        ETConstr(s,bko,(n,InternalProd))
      case (Some(ETPattern(b,n)),_) => ETPattern(b,n)
      case (Some(ETConstr(s,bko,None)),_) => ETConstr(s,bko,typ)
      case (Some(ETIdent),_) => ETIdent
      case (Some(ETBigint),_) => ETBigint
      case (Some(ETGlobal),_) => ETGlobal
      case (Some(ETBinder(isOpenBindersList)),_) => ETBinder(isOpenBindersList)
    }
    (x,resultTyp)
  }

 /* //TODO: Implement
  def precedence_of_entry_type(entry: NotationEntry, i: Int) :(String,ConstrEntryKeyGen[(ProductionLevel,ProductionPosition)]) => _ = {
    null
  }

  def assoc_of_type(entry: interpTypes.constrExpr.NotationEntry, i: Int):(String,ConstrEntryKeyGen[(ProductionLevel,ProductionPosition)]) => _ = {
    (_,typ) => precedence_of_entry_type(entry,i)(typ)
  }*/

  //TODO:Implement
  def find_subentry_types(entry: interpTypes.constrExpr.NotationEntry,
                          i: Int,
                          maybeAssoc: Option[gramlibTypes.gramext.GAssoc],
                          etyps: List[(kernelTypes.id.T, parsingTypes.extend.SimpleConstrProdEntryKey)],
                          symbols: List[notationTypes.Symbol]) = {
    val innerLevel = NumLevel(200)
    val typs = find_symbols(
      (NumLevel(i),BorderProd(parsingTypes.extend.Left,maybeAssoc)),
      (innerLevel,InternalProd),
      (NumLevel(i),BorderProd(parsingTypes.extend.Right,maybeAssoc)))(symbols)
    val mapper = set_entry_type(entry,etyps)
    val sy_typs = typs.map{tup => set_entry_type(entry,etyps)(tup._1,tup._2)}
    //val prec = assoc_of_type(entry,i)
  }

  type SubentryTypes = List[(kernelTypes.id.T,ConstrEntryKey)]

  sealed case class SynData(info:Notation,
                            only_parsing:Boolean,
                            only_printing:Boolean,
                            deprecation:Option[interpTypes.deprecation.T],
                            format:Option[String],
                            extra:List[(String,String)],
                            recvars:List[(kernelTypes.id.T,kernelTypes.id.T)],
                            mainvars:List[String],
                            intern_typs:List[NotationVarInternalizationType],
                            //TODO:not original type
                            level:(NotationEntry,Int),
                           //Note: Associativity was added manually
                            pa_syntax_data:(SubentryTypes,List[Symbol],NotationModifier),
                            not_data:(Notation,Boolean)
                           )

  /**
    *
    * @param df
    * @param modifiers
    * @todo Add pretty printing information
    */
  def compute_syntax_data(df:String,modifiers:List[SyntaxModifier])(implicit debugPrint:Boolean = false): SynData = {
    if(debugPrint)println("[Debug print activated, starting to compute syntax]")
    val mods = interp_modifiers(modifiers)
    if(debugPrint)println("interpreted modifiers: " + mods)
    if (mods.only_printing && mods.only_parsing)
      throw new IllegalStateException("A notation cannot be both 'only printing' and 'only parsing'.")
    val assoc = if(mods.assoc.isDefined) mods.assoc else Some(NonA)
    if(debugPrint)println("Determined Associativity: " + assoc)
    val newMods = mods.copy(assoc = assoc)
    val analyzation = analyze_notation_tokens(df,mods.only_printing)(debug = debugPrint)
    if(debugPrint)println("Recursive variable: " + analyzation._1 + " , Main variables: " + analyzation._2 + " , symbols: " + analyzation._3)
    //TODO: implement checks
    val precedence = find_precedence(mods.custom,mods.level,mods.eTypes,analyzation._3,mods.only_printing)
    if(debugPrint) println("Found precedence: " + precedence)
    val custom = make_custom_entry(mods.custom,precedence._2)
    if(debugPrint) println("Notation to be inserted into: " + custom)
    val ntn_for_interp = make_notation_key(custom,analyzation._3)
    if (debugPrint) println("Notation key: " + ntn_for_interp)
    val symbols_for_grammar = if (custom equals InConstrEntrySomeLevel) remove_curly_brackets(analyzation._3) else analyzation._3
    if(debugPrint) println("Notation after removing curly parenthesization: " + symbols_for_grammar)
    val need_squash = !(symbols_for_grammar equals analyzation._3)
    if(debugPrint && need_squash) println("Squash needed")
    val ntn_for_grammar = if(need_squash)make_notation_key(custom,symbols_for_grammar) else ntn_for_interp
    if(debugPrint) println("Notation for grammar: " + ntn_for_grammar)
    if ((mods.custom equals InConstrEntry) && !mods.only_printing) check_rule_productivity(symbols_for_grammar)
    val etyps = join_auxiliary_recursive_types(analyzation._1,mods.eTypes)
    if(debugPrint) println("Etypes for globalization: " + etyps)
    //TODO:Implement this part, unrelevant for now
   // val subentryTypes = find_subentry_types(mods.custom,precedence._2,assoc,etyps,analyzation._3)
    SynData(ntn_for_interp,
      newMods.only_parsing,
      newMods.only_printing,
      None,
      newMods.format,
      newMods.extra,
      analyzation._1,
      analyzation._2,
      Nil,
      (newMods.custom,precedence._2),
      (Nil,symbols_for_grammar,newMods),
      (ntn_for_grammar,need_squash)
    )

  }
}
