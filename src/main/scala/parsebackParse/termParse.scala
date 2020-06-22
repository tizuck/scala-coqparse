package parsebackParse

import ParserFix._
import model.{interpTypes, kernelTypes, libraryTypes, metasyntax, notationTypes, pretypingTypes}
import parseback._
import implicits._
import model.engineTypes.evarKinds
import model.interpTypes.constrExpr._
import model.interpTypes.constrExpr.constructor._
import model.engineTypes.namegen.{IntroAnonymous, IntroFresh, IntroIdentifier}
import model.gramlibTypes.gramext
import model.gramlibTypes.gramext.{LeftA, NonA, RightA}
import model.interpTypes.HVals._
import model.interpTypes.notationTerm.ETConstr
import model.kernelTypes.id
import model.kernelTypes.name.Name
import model.kernelTypes.constr.{LetPatternStyle, RegularStyle}
import model.kernelTypes.id.T
import model.libraryTypes.libNames.Qualid
import model.metasyntax.{NotationModifier, SynData}
import model.notationTypes.{NonTerminal, Terminal}
import model.parsingTypes.extend.{NumLevel, SimpleConstrProdEntryKey}
import model.pretypingTypes._
import model.pretypingTypes.globTerm._
import model.vernacTypes.vernacExpr.{OnlyParsingFlag, SyntacticDefinition, SyntaxExtension}
import org.bitbucket.inkytonik.kiama.==>
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import parseback.Parser.Failure
import parsebackParse.termParse.HTypes.HInst


//TODO: Substitute .** through an option with a list with at least 1 element or no list at all. Implementation of .** causes multiple identical results in final list
object termParse {

  implicit val W:parseback.Whitespace = Whitespace("""\s+""".r.?)

  /**
    * Interface for rules relying on precedences.
    *
    * Implemented by [[OprConstr]] and [[Pattern]]
    *
    * @tparam T The result-type of the Parser
    */
  sealed trait Constr [T]{

    protected implicit val W: parseback.Whitespace = parseback.Whitespace("""\s+""".r.?)

    /**
      * Short-type for tuples (precedence, parser) stored in a Map
      */
    type ParserTable = scala.collection.mutable.Map[Int,(Boolean, Parser[T])]

    /**
      * Upon construction of Parsers to precedences by using the [[initParTab()]]-method
      * this value stores a parser for each precedence.
      */
    protected val parserTable: ParserTable = scala.collection.mutable.HashMap.empty[Int,(Boolean,Parser[T])]

    //TODO:Make protected later on
    val reservedNotationExtensions: scala.collection.mutable.Map[Int,Parser[T]] = scala.collection.mutable.HashMap.empty

    /**
      * Constructs parsers for each precedence and all precedences below and stores them in
      * [[parserTable]]. After constructing the parserTable it combines all parsers matching a precedence <= prec
      * and returns the combined parser.
      *
      * Computation of parsers is only executed once using the [[scala.collection.mutable.HashMap.getOrElseUpdate()]]
      * -method
      *
      * Therefore it is strongly recommended to only use the apply-method once calling the [[initParTab()]]-method
      * as construction of parser-table is expensive
      *
      * @param prec precedence
      * @return a parser for the given precedence
      */
    final def apply(prec:Int):Parser[T] = {
      val pEntry = parserTable.get(prec)
      //, recurseIfG0(prec, defaultRules(prec), reservedNotationExtensions.get(prec))
      if(pEntry.isEmpty){
        val newEntry = recurseIfG0(prec,defaultRules(prec),reservedNotationExtensions.get(prec))
        parserTable.update(prec,(false,newEntry))
        newEntry
      } else {
        pEntry.get._2
      }
    }

    final def markDirty(prec:Int):Unit = {
      parserTable.clear()
    }

    /**
      * collection of rules for each precedence possibly consisting of cross-referencing rules.
      *
      * If rules of this type are used within sub-rules they should be referenced using this(prec) so
      * multiple constructions of [[parserTable]] are avoided
      *
      * @param prec precedence
      * @return Parser consisting of specific rules for precedence level
      */
    def defaultRules(prec:Int): Parser[T]

    /**
      * Returns final parser by walking through all precedences and combining the parsers constructed for each
      * precedence-level
      *
      * @param prec precedence
      * @param defaultP Parser containing the default rules for specific precedence level
      * @return final parser for precedence
      */
    protected final def recurseIfG0(prec:Int, defaultP : => Parser[T],oExtension : => Option[Parser[T]]) : Parser[T] =
      oExtension match {
      case Some(extensionP) => if (prec > 0){extensionP | defaultP | this(prec - 1)} else extensionP | defaultP
      case None             => if (prec > 0) {defaultP | this (prec - 1)} else defaultP
    }

    /**
      * Should be used to initialize the complete [[parserTable]] once.
      *
      * It does this by calling the apply method and passing the highest possible precedence
      */
    final def initParTab():Unit = apply(200)
  }

  sealed case class OprConstr(/*notation: Int => OprConstr => Parser[ConstrExpr]*/) extends Constr[ConstrExpr] {

    type SyntacticDefinitionKey = kernelTypes.id.T
    type SyntacticDefinitionEntry = (List[kernelTypes.id.T],ConstrExpr,OnlyParsingFlag)
    type SyntacticDefinitionTable = scala.collection.mutable.HashMap[SyntacticDefinitionKey,SyntacticDefinitionEntry]

    val syntacticDefinitionTable : SyntacticDefinitionTable = scala.collection.mutable.HashMap.empty

    private def validateNonAssociativity(symbols: List[notationTypes.Symbol]):Unit = {
      symbols.head match {
        case NonTerminal(id) => throw new IllegalStateException("Notation can not be non-associative but at the same time have a leftmost operant.")
        case _ => symbols.last match {
          case NonTerminal(id) => throw new IllegalStateException("Notation can not be non-associative but at the same time have a rightmost operant.")
          case _ =>
        }
      }
    }

    private def validateRightAssociativity(symbols: List[notationTypes.Symbol]):Unit = {
      symbols.last match {
        case NonTerminal(id) =>
        case _ => throw new IllegalStateException("No terminal symbol expected for last symbol if rule is right associative.")
      }
    }

    private def validateLeftAssociativity(symbols: List[notationTypes.Symbol]):Unit = {
      symbols.head match {
        case NonTerminal(id) =>
        case _ => throw new IllegalStateException("No terminal symbol expected for last symbol if rule is right associative.")
      }
    }

    sealed trait EntryStatus
    case object NoEntryFound extends EntryStatus
    case object EntryFound extends EntryStatus

    private def findLevelForId(entryForId: Option[(T, SimpleConstrProdEntryKey)]): (Int,EntryStatus) = {
      entryForId match {
        case Some(value) =>
          value match {
            //TODO:This is far from exhaustive. Apply more notations for new cases
            case (_, ETConstr(InConstrEntry, None, Some(NumLevel(n)))) =>
              (n,EntryFound)
          }
        //Assuming default level is set to 200
        case None => (200,NoEntryFound)
      }
    }

    private def buildSyntaxExtensionNonAParserRule(symbols:List[model.notationTypes.Symbol], interpretedModifiers:NotationModifier):Parser[ConstrExpr] = symbols match {
      case x :: Nil =>
        x match {
          //Last symbol may not be a Non-Terminal
          case Terminal(s) => s ^^ ReservedNotation()
          case _ => throw new IllegalStateException("Last symbol may not be a Non-Terminal.")
        }
      case head :: tail => head match {
        case NonTerminal(id) =>
          val entryForId = interpretedModifiers.eTypes.find(pair => pair._1 equals id)
          val levelForId = findLevelForId(entryForId)
          //Build parser subrule
          this(levelForId._1) ~ buildSyntaxExtensionNonAParserRule(tail,interpretedModifiers) ^^ propagateReservedNotation
        case Terminal(s) => s ~> buildSyntaxExtensionNonAParserRule(tail,interpretedModifiers)
        case _ => throw new NotImplementedError()
      }

    }

    private def buildSyntaxExtensionInnerParserRule(symbols:List[notationTypes.Symbol],synData: SynData):Parser[ConstrExpr] = {
      def build(symbol:notationTypes.Symbol):Either[Parser[String],Parser[ConstrExpr]] = symbol match {
        case Terminal(s) => Left(s:Parser[String])
        case NonTerminal(id) =>
          val level = findLevelForId(synData.pa_syntax_data._3.eTypes.find(pair => pair._1 equals id))
          level match {
            case (n,_) => Right(this(n))
          }
        case notationTypes.SProdList(id, symbols) => throw new NotImplementedError()
        case notationTypes.Break(n) =>throw new NotImplementedError()
      }
      symbols match {
        case symbol :: Nil => build(symbol) match {
          case Left(s) => s ^^ ReservedNotation()
          case Right(c) => c ^^ {(_,c) => ReservedNotation(List(c))}
        }
        case symbol :: tail => build(symbol) match {
          case Left(s) =>s ~> buildSyntaxExtensionInnerParserRule(tail,synData)
          case Right(c) => c ~ buildSyntaxExtensionInnerParserRule(tail,synData) ^^ propagateReservedNotation
        }
      }
    }

    private def outmostSyntaxExtensionAssociativeParser(symbols: List[notationTypes.Symbol], ruleLevel: RawNaturalNumber,synData: SynData) = {
      symbols.head match {
        case NonTerminal(id) =>
          val level = findLevelForId(synData.pa_syntax_data._3.eTypes.find(pair => pair._1 equals id))
          level match {
            case (n, EntryFound) if n < ruleLevel => throw new IllegalStateException("Rightmost operand of a" +
              " right-associative parser rule may not have a lower precedence than rule level.")
            case (n, EntryFound) if n >= ruleLevel => this (n)
            case (_, NoEntryFound) => this(ruleLevel)
          }
      }
    }

    private def outmostSyntaxExtensionNonAssociativeParser(symbols: List[notationTypes.Symbol], ruleLevel: RawNaturalNumber,synData: SynData) = {
      symbols.last match {
        case Terminal(s) => Left(s: Parser[String])
        case NonTerminal(id) =>
          val level = findLevelForId(synData.pa_syntax_data._3.eTypes.find(pair => pair._1 equals id))
          level match {
            case (_, NoEntryFound) =>
              //No Id found, needs to be set to the appropriate level manually
              Right(this(ruleLevel - 1))
            case (n, EntryFound) if n >= ruleLevel => throw new IllegalStateException("Right associative rule can not also be left associative" +
              " (Level for left-most operand is too high).")
            case (n, EntryFound) => Right(this(n))
          }
        case _ => throw new NotImplementedError()
      }
    }

    private def buildSyntaxExtensionLeftAParserRule(symbols: List[notationTypes.Symbol], synData: metasyntax.SynData):Parser[ConstrExpr] = {
      val ruleLevel = synData.level._2
      val leftmostParser = outmostSyntaxExtensionAssociativeParser(symbols, ruleLevel, synData)
      val rightmostParser = outmostSyntaxExtensionNonAssociativeParser(symbols, ruleLevel, synData)

      val innerSymbols = symbols.tail.reverse.tail.reverse
      val innerParser = buildSyntaxExtensionInnerParserRule(innerSymbols,synData)

      rightmostParser match {
        case Left(s) => (leftmostParser ~ innerParser ^^ propagateReservedNotation) <~ s
        case Right(c) => (leftmostParser ~ innerParser ^^ propagateReservedNotation) ~ c ^^ appendRightmostParser
      }
    }

    private def buildSyntaxExtensionRightAParserRule(symbols:List[model.notationTypes.Symbol], synData:SynData):Parser[ConstrExpr] = {
      val ruleLevel = synData.level._2
      val leftmostParser = outmostSyntaxExtensionNonAssociativeParser(symbols, ruleLevel, synData)
      val rightmostParser = outmostSyntaxExtensionAssociativeParser(symbols, ruleLevel, synData)

      val innerSymbols = symbols.tail.reverse.tail.reverse
      val innerParser = buildSyntaxExtensionInnerParserRule(innerSymbols,synData)

      val par:Parser[ConstrExpr] =  leftmostParser match {
        case Left(s) => s ~> innerParser
        case Right(c) => c ~ innerParser ^^ propagateReservedNotation
      }

      par ~ rightmostParser ^^ appendRightmostParser
    }

    private def buildSyntaxExtensionParser(symbols:List[model.notationTypes.Symbol], synData: SynData): Parser[ConstrExpr] = synData.pa_syntax_data._3.assoc match {
      case Some(gramext.NonA) =>
        validateNonAssociativity(symbols)
        buildSyntaxExtensionNonAParserRule(symbols,synData.pa_syntax_data._3)
      case Some(gramext.RightA) =>
        validateRightAssociativity(symbols)
        buildSyntaxExtensionRightAParserRule(symbols,synData)
      case Some(gramext.LeftA) =>
        validateLeftAssociativity(symbols)
        buildSyntaxExtensionLeftAParserRule(symbols,synData)
      case _ => throw new IllegalStateException("Associativity should be initialized by notationModifier.")
    }
    /**
      * Adds a syntactic definition for the current file to the parser.
      *
      * Note that a syntactic definition is an ident and is always parsed as a qualid. Therefore
      * this parser safes the substitutes in a map for evaluation purposes.
      *
      * @param syntacticDefinition
      * @throws IllegalStateException if an identifier is allready associated with a key
      */
    def addSyntacticDefinition(syntacticDefinition: SyntacticDefinition) :Unit  = {
      val trip = (syntacticDefinition.idsTermAssoc._1,syntacticDefinition.idsTermAssoc._2,syntacticDefinition.onlyParsing)
      if (syntacticDefinitionTable.contains(syntacticDefinition.id))
        throw new IllegalArgumentException("Multiple Notations with identifier: " + syntacticDefinition.id)
      syntacticDefinitionTable.update(syntacticDefinition.id,trip)
    }

    def addSyntaxExtension(syntaxExtension: SyntaxExtension):Unit = {

      val synData = model.metasyntax.compute_syntax_data(syntaxExtension.ssyntaxMods._1,syntaxExtension.ssyntaxMods._2)(debugPrint = false)

      val parserToInsert = buildSyntaxExtensionParser(synData.pa_syntax_data._2,synData)
      val levelToInsert = synData.level._2

      makeParserEntry(parserToInsert, levelToInsert)
      markDirty(levelToInsert)
    }

    private def makeParserEntry(parserToInsert: Parser[ConstrExpr], levelToInsert: RawNaturalNumber): Unit = {
      if (reservedNotationExtensions.contains(levelToInsert)) {
        val oldParserTableEntry = reservedNotationExtensions(levelToInsert)
        reservedNotationExtensions.update(levelToInsert,oldParserTableEntry | parserToInsert)
      } else {
        reservedNotationExtensions += (levelToInsert -> parserToInsert)
      }
    }


    object strategies {
      import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

      /**
        * @param term current state of substitution
        * @param strat strat to be applied to the subterm
        */
      def evalConstrExpr(term:ConstrExpr)(strat:Strategy):ConstrExpr = term match {
        case App(a@_,b@_) =>
          val rewrittenList = b.map(tup => (rewrite[ConstrExpr](strat)(tup._1),tup._2))
          App((a._1,rewrite[ConstrExpr](strat)(a._2)),rewrittenList)
        case r@Ref(_,_) => r
        case ProdN(bll,c) =>
          val rewrittenList = bll.map{
            b =>  evalLocalBinderExprInConstrExprContext(b)(strat)
          }
          ProdN(rewrittenList,rewrite(strat)(c))
      }

      def evalLocalBinderExprInConstrExprContext(term:LocalBinderExpr)(strat:Strategy):LocalBinderExpr = term match {
        case LocalAssum(names, b, c) => LocalAssum(names,b,rewrite[ConstrExpr](strat)(c))
        case LocalDef(n, c1, oC2) => LocalDef(n,rewrite[ConstrExpr](strat)(c1),oC2.map{c => rewrite(strat)(c)})
        case LocalPattern(tu) => LocalPattern((tu._1,tu._2.map{c => rewrite(strat)(c)}))
      }

      val substituteStrategy: ConstrExpr => kernelTypes.id.T => Strategy = c => id => rule ({
        case Ref(q,None) if q.basename == id => c
        case _c@_ => evalConstrExpr(_c)(substituteStrategy(c)(id))
      }:ConstrExpr ==> ConstrExpr)

    }

    //TODO: Interpret non associative interpretation in Coq
    def buildNotationNonAParser(symbols:List[model.notationTypes.Symbol], synData:SynData):Parser[ConstrExpr] = {

      val ruleLevel = synData.level._2
      val leftmostParser = symbols.head match {
        case Terminal(s) => s:Parser[String]
        case _ => throw new NotImplementedError()
      }

      val rightmostParser = symbols.last match {
        case Terminal(s) => s:Parser[String]
        case _ => throw new NotImplementedError()
      }

      def buildInnerParser(symbols:List[notationTypes.Symbol],
                           synData: SynData):Parser[interpTypes.constrExpr.constructor.NotationEntry] = symbols match {
        case x :: Nil => x match {
          case NonTerminal(id) =>
            val levelForEntry = defineEntryLevel(synData, id)
            this(levelForEntry) ^^ {
              (_,c) => NotationEntry(c :: Nil,strategies.substituteStrategy(c)(id))
            }
          case Terminal(s) =>
            s ^^ NotationEntry()
        }
        case head :: tail => head match {
          case NonTerminal(id) =>
            val levelForEntry = defineEntryLevel(synData, id)
            this(levelForEntry) ~ buildInnerParser(tail,synData) ^^ {
              (_,c,not) => not match {
                case NotationEntry(constrs, rewritingStrategy) =>
                  NotationEntry(c :: constrs, strategies.substituteStrategy(c)(id) <* rewritingStrategy)
              }
            }
          case Terminal(s) =>
            s ~> buildInnerParser(tail,synData)
        }
      }

      leftmostParser ~> buildInnerParser(symbols.tail.reverse.tail.reverse,synData) <~ rightmostParser
    }

    def buildNotationRightAParser(symbols:List[notationTypes.Symbol], data: metasyntax.SynData):Parser[ConstrExpr] = {
      val leftmostParser = outmostNotationNonAParser(symbols,data)
      val rightmostParser = outMostAssociativeNotationParser(symbols,data)

      leftmostParser match {
        case Left(f) => f(buildInnerNotationAssociativeParser(symbols.tail.reverse.tail.reverse,data) ~ rightmostParser ^^ {
          (_,c1,c2) => NotationEntry(c1.constrs ++ c2.constrs,c1.rewritingStrategy <* c2.rewritingStrategy)
        })
        case Right(c) =>c ~ buildInnerNotationAssociativeParser(symbols.tail.reverse.tail.reverse,data) ~ rightmostParser ^^ {
          (_,c1,c2,c3) =>
            NotationEntry(c1.constrs ++ c2.constrs ++ c3.constrs,
            c1.rewritingStrategy <* c2.rewritingStrategy <* c3.rewritingStrategy)
        }
      }
    }

    private def buildInnerNotationAssociativeParser(symbols: List[notationTypes.Symbol], data:SynData):Parser[interpTypes.constrExpr.constructor.NotationEntry] = symbols match {
      case x :: Nil => x match {
        case NonTerminal(id) =>
          val level = defineEntryLevel(data,id)
          this(level) ^^ {(_,c) => NotationEntry(c::Nil,strategies.substituteStrategy(c)(id))}
        case Terminal(s) =>
          s ^^ NotationEntry()
      }
      case head :: tail => head match {
        case NonTerminal(id) =>
          val level = defineEntryLevel(data,id)
          this(level) ~ buildInnerNotationAssociativeParser(tail,data) ^^ {(_,c,c2) => c2 match {
            case NotationEntry(constrs, rewritingStrategy) =>
              NotationEntry(c::constrs,strategies.substituteStrategy(c)(id) <* rewritingStrategy)
          }}
        case Terminal(s) =>
          s ~> buildInnerNotationAssociativeParser(tail,data)
      }
    }

    def buildNotationLeftAParser(symbols: List[notationTypes.Symbol], data: metasyntax.SynData):Parser[ConstrExpr] = {

      val leftMostParser = outMostAssociativeNotationParser(symbols, data)
      val rightMostParser = outmostNotationNonAParser(symbols, data)

      rightMostParser match {
        case Left(f) => f(leftMostParser ~ buildInnerNotationAssociativeParser(symbols.tail.reverse.tail.reverse,data) ^^ {
          (_,c1,c2) => NotationEntry(c1.constrs ++ c2.constrs, c1.rewritingStrategy <* c2.rewritingStrategy)
          })
        case Right(c) => leftMostParser ~ buildInnerNotationAssociativeParser(symbols.tail.reverse.tail.reverse,data) ~ c ^^ {
          (_,c1,c2,c3) =>
                NotationEntry(c1.constrs ++ c2.constrs ++ c3.constrs,
                  c1.rewritingStrategy <* c2.rewritingStrategy <* c3.rewritingStrategy)}
      }
    }

    private def outmostNotationNonAParser(symbols: List[notationTypes.Symbol], data: SynData):
    Either[Parser[constructor.NotationEntry] => Parser[constructor.NotationEntry], Parser[constructor.NotationEntry]] = {
      symbols.last match {
        case NonTerminal(id) =>
          val level = defineEntryLevel(data, id)
          Right(this (if (level >= data.level._2) level - 1 else level) ^^ { (_, c) => NotationEntry(List(c), strategies.substituteStrategy(c)(id)) })
        case Terminal(s) => Left((p: Parser[constructor.NotationEntry]) =>
          data.pa_syntax_data._3.assoc match {
            case Some(a) =>
              a match {
                case LeftA => p <~ s
                case RightA => s ~> p
              }
            case None => s ~> p
          })
      }
    }

    private def outMostAssociativeNotationParser(symbols: List[notationTypes.Symbol], data: SynData) = {
      symbols.head match {
        //Left Associative rule allways has a leftmost variable, therefore no other cases need to be examined
        case NonTerminal(id) =>
          val level = defineEntryLevel(data, id)
          this (if (level < data.level._2) data.level._2 else level) ^^ { (_, c) => NotationEntry(c :: Nil, strategies.substituteStrategy(c)(id)) }
      }
    }

    private def defineEntryLevel(synData: SynData, id: T) = {
      findLevelForId(synData.pa_syntax_data._3.eTypes.find(pair => pair._1 equals id)) match {
        case (_, NoEntryFound) => 200
        case (n, EntryFound) => n
      }
    }

    def buildNotationParser(symbols:List[model.notationTypes.Symbol], synData: SynData):Parser[ConstrExpr] =
      synData.pa_syntax_data._3.assoc.getOrElse(NonA) match {
        case gramext.NonA =>
          validateNonAssociativity(synData.pa_syntax_data._2)
          buildNotationNonAParser(symbols,synData)
        case gramext.RightA =>
          validateRightAssociativity(synData.pa_syntax_data._2)
          buildNotationRightAParser(synData.pa_syntax_data._2,synData)
        case gramext.LeftA =>
          validateLeftAssociativity(synData.pa_syntax_data._2)
          buildNotationLeftAParser(symbols,synData)
    }

    def addNotation(notation:model.vernacTypes.vernacExpr.Notation):Unit = {
      val synData = model.metasyntax.compute_syntax_data(notation.ssyntaxMods._1,notation.ssyntaxMods._2)(debugPrint = true)
      val parser = buildNotationParser(synData.pa_syntax_data._2,synData)
      val levelToInsert = synData.level._2
      makeParserEntry(parser,levelToInsert)
      markDirty(levelToInsert)
    }

    private def appendRightmostParser:(List[Line],ConstrExpr,ConstrExpr) => ConstrExpr = {
      (_, reservedNotation, c) =>
        reservedNotation match {
          case ReservedNotation(constrExprs) => ReservedNotation(constrExprs ++ (c :: Nil))
        }
    }

    private def propagateReservedNotation:(List[Line],ConstrExpr,ConstrExpr) => ConstrExpr = {
      (_, c1, c2) =>
        c2 match {
          case ReservedNotation(constrExprs) => ReservedNotation(c1 :: constrExprs)
          case _ => throw new IllegalStateException("Nothing other than ReservedNotation allowed.")
        }
    }

    /**
      * Default rules cover rules for term. In Detail:
      *
      * For precedence-level 200 look at [[binder_constr]]
      *
      * Precedence-level 100 covers the following rules from the reference manual:
      *
      * {{{ term : term
      *   | term <: term
      *   | term :>
      *   | term <: term
      * }}}
      *
      * Precedence-level 10 covers the following: {{{
      *   term arg ... arg
      *   |@ qualid [term ... term]
      * }}}
      *
      * Implementation of rule '''|@ qualid [term ... term]''' differs from the reference manual. [[global]] references
      * specific kinds of qualid. Here access_idents are solved by implicitly placing part behind dot in a lower precedence.
      * [[instance]] is not mentioned in the reference manual. It allows for application as the following:
      *
      * {{{@a @{b:(forall n:nat,...)} c d}}}
      *
      * Precedence-level 9 defines a rule that is not referenced at all but found in g_constr.mlg
      *
      * Precedence-level 1 defines rule
      *
      * {{{
      *   | term % ident
      *
      * }}}
      *
      * and solves the acces_ident syntax
      *
      * Precedence-level 0 defines atomic rules. It covers terms in parenthesis, case analysis and further atomic values
      * referenced at [[atomic_constr]]. In Detail:
      *
      * {{{
      *    | match match_item , … , match_item [return_type] with
      *                     [[|] equation | … | equation] end
      *                   | qualid
      *                   | sort
      *                   | num
      *                   | _
      *                   | ( term )
      * }}}
      *
      * @param prec precedence
      * @return Parser consisting of specific rules for precedence level
      */
    override def defaultRules(prec: Int): Parser[ConstrExpr] = {
      prec match {
        case 200 => binder_constr
        case 100 => this.apply(99) ~ "<:" ~ binder_constr ^^ { (_, c1, _, c2) => Cast(c1,CastVM(c2)) } |
          this (99) ~ "<:" ~ this (100) ^^ { (_, c1, _, c2) => Cast(c1,CastVM(c2)) } |
          this (99) ~ "<<:" ~ binder_constr ^^ { (_, c1, _, c2) => Cast(c1,CastNative(c2)) } |
          this (99) ~ "<<:" ~ this (100) ^^ { (_, c1, _, c2) => Cast(c1,CastNative(c2)) } |
          this (99) ~ ":" ~ binder_constr ^^ { (_, c1, _, c2) => Cast(c1,CastConv(c2)) } |
          this (99) ~ ":" ~ this (100) ^^ { (_, c1, _, c2) => Cast(c1,CastConv(c2)) } |
          this (99) ~ ":>" ^^ { (_, c, _) => Cast(c,CastCoerce)}
        case 10 => this(9) ~ appl_arg.++ ^^ {
          (_,c,ags) => {
            App((None,c),ags)
          }
        } |
          "@" ~ global ~? instance ~? this(9).++ ^^ {
            (_,_,g,i,cs) => {
              cs match {
                case Some(_cs) => AppExpl((None,g,i match {case Some(_i) => _i case None => None}),_cs)
                case None => AppExpl((None,g,i match {case Some(_i) => _i case None => None}),Nil)
              }
            }
          } |
          "@" ~ primParse.pattern_identref ~ primParse.identref.++ ^^ {
            (_,_,i,is) => {
              val args = is.map(i => (Ref(libraryTypes.libNames.qualid_of_ident(i),None),None))
              App((None,PatVar(i)),args)
            }
          }
        case 9 => ".." ~> this (0) <~ ".." ^^ {
          (_,c) => AppExpl((None,libraryTypes.libNames.qualid_of_ident(".."),None),List(c))
        }
        case 1 => this (0) ~ ".(" ~ global ~ appl_arg.++ ~ ")" ^^ {
          (_,c,_,q,args,_) => {
            App((Some(args.size + 1),Ref(q,None)),args ++ List((c,None)))
          }
        } | this(0) ~ ".(" ~ global ~ ")" ^^ {
          (_,c,_,q,_) => App((Some(1),Ref(q,None)),List((c,None)))
        } |
          this (0) ~ ".(" ~ "@" ~ global ~? this (9).++ ~ ")" ^^ {
            (_:List[Line],
              tu:(ConstrExpr,String),
            _:String,
            q:Qualid,
            cs:Option[List[ConstrExpr]],
            _:String) => cs match {
              case Some(_cs) => AppExpl((Some(cs.size + 1),q,None),_cs++List(tu._1))
              case None => AppExpl((Some(cs.size + 1),q,None),List(tu._1))
            }
          } |
          this (0) ~ "%" ~ primParse.ident ^^ {
            (_,c,_,i) => Delimiters(i,c)
          }
        case 0 => atomic_constr |
          match_constr |
          "(" ~> this(200) <~ ")" ^^ { (_, c) => {
            c match {
              case Prim(Numeral(_,true)) => Notation((InConstrEntrySomeLevel,"(_)"),(List(c),Nil,Nil,Nil))
              case _ => c
            }
          } } |
          "{|" ~> record_declaration <~ "|}" |
          "{|" ~ "|}" ^^ {(_,_,_) => Record(Nil)} |
          "{" ~> binder_constr <~ "}" ^^ { (_, c) => Notation((InConstrEntrySomeLevel,"( _ )"),(List(c),Nil,Nil,Nil)) } |
          "`{" ~> this (200) <~ "}" ^^ { (_, c) => Generalization(Implicit,None,c) } |
          "`(" ~> this (200) <~ ")" ^^ { (_, c) => Generalization(Explicit,None,c)}
        case _ => Parser.Failure(List.empty)
      }
    }
  }

  sealed case class Pattern() extends Constr[CasesPatternExpr] {

        override def defaultRules(prec: Int): Parser[CasesPatternExpr] = {
          prec match {
            case 100 => this (99) ~ "|" ~ this (100) ~? ("|" ~ this (100)).++ ^^ {
              (_,cp1:CasesPatternExpr,_:String,cp2:CasesPatternExpr,cps) =>
                cps match {
                  case Some(_cps) => PatOr(cp1 :: cp2 :: _cps.map(_._2))
                  case None => PatOr(cp1 :: cp2 :: Nil)
                }
            }
            case 10 => (this (10) ~ "as" ~ primParse.name ^^ {
              (_,cP,_,n) => PatAlias(cP,n)
            }
              | this (10) ~ this (9).++ ^^ {
              (_,cp,cps) => interpTypes.constrExprOps.mkAppPattern(cp)(cps)
            }
              | "@" ~ primParse.reference ~? this(9).++ ^^ {
              (_,_,q,cps) =>
                cps match {
                  case Some(_cps) => PatCstr(q,Some(_cps),Nil)
                  case None => PatCstr(q,Some(Nil),Nil)
                }
          })
            case 1 => this (1) ~ "%" ~ primParse.ident ^^ {
              (_,cp,_,i) => PatDelimiters(i,cp)
            }
            case 0 => (primParse.reference ^^ {(_,q) => PatAtom(Some(q))}
              | "{|" ~ record_patterns ~ "|}" ^^ {(_,_,rps,_) => PatRecord(rps)}
              | "{|" ~ "|}" ^^ {(_,_,_) => PatRecord(Nil)}
              | "_" ^^ {(_,_) => PatAtom(None)}
              |"(" ~ this (200) ~ ")" ^^ {(_,_,cp,_) => cp match {
                case PatPrim(Numeral(_,true)) => PatNotation((InConstrEntrySomeLevel,"( _ )"),(List(cp),Nil),Nil)
                case _ => cp
            }}
              |"(" ~ this (200) ~ ":" ~ lconstr ~ ")" ^^ {
              (_,_,cp,_,c,_) => {
                val p = cp match {case PatPrim(Numeral(_,true)) => PatNotation((InConstrEntrySomeLevel,"( _ )"),(List(cp),Nil),Nil)
                case _ => cp
                }
                PatCast(p,c)
              }
            }
              |primParse.int ^^ {(_,n) => PatPrim(Numeral(n,s = true))}
              |primParse.string ^^ {(_,s) => PatPrim(SString(s))}
          )
            case _ => Parser.Failure(List.empty)
          }
        }
  }

  val pattern = Pattern()
  pattern.initParTab()

  val oprConstr = OprConstr()

  lazy val global : Parser[Qualid] = primParse.reference

  lazy val constr_pattern : Parser[ConstrExpr] = constr

  lazy val lconstr_pattern : Parser[ConstrExpr] = lconstr

  lazy val record_declaration : Parser [ConstrExpr]= record_fields ^^ { (_, rf) => Record(rf)}

  lazy val lconstr : Parser[ConstrExpr] = oprConstr(200)

  lazy val record_field_declaration : Parser[(Qualid,ConstrExpr)]  =
    global ~? binders ~ ":=" ~ lconstr ^^ {(_,q,b,_,c) =>
      b match {
        case Some(_b) => (q,mkLambdaCN(_b)(c))
        case None => (q,mkLambdaCN(Nil)(c))
      }
    }

  //TODO: Erase Helpertypes, eventually they are unnecessary
  lazy val inst : Parser[HInst] = primParse.ident ~ ":=" ~ lconstr ^^ {
    (_,i,_,c) => HInst(i,c)
}

  lazy val evar_instance : Parser[List[HInst]] =
    "@{" ~ inst ~? (";" ~ inst).++ ~ "}" ^^ {
    (_,_,i,is,_) => is match {
      case Some(_is) => i :: _is.map(i => i._2)
      case None => i :: Nil
    }
  }


  /**
    * Part of Cofix and Fix statements in rule '''fix_constr'''
    */
  lazy val single_fix:Parser[(Boolean,(id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr]))] = fix_kw ~ fix_decl ^^ { (_, kw, dcl)=>(kw,dcl)}

  /**
    * References the following rules from the reference manual
    *
    * {{{fix_body     ::=  ident binders [annotation] [: term] := term
    * cofix_body    ::=  ident [binders] [: term] := term
    * }}}
    *
    * '''typ_cstr''' ist used to cover the '''[: term]''' part.
    */
  lazy val fix_decl:Parser[(id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr])] =
    primParse.identref ~? opt_binders_fixannot ~? opt_type_cstr ~ ":=" ~ oprConstr(200) ^^ {
    (_,id,fixb,ty,_,c) => {
      fixb match {
        case Some(_fixb) => (id, _fixb._1,_fixb._2, c,ty match {case Some(_ty) => _ty case None => None})
        case None => (id,Nil,None,c,ty match {case Some(_ty) => _ty case None => None})
      }
    }
  }

  lazy val match_constr : Parser[Cases] =
    "match" ~ case_item ~? ("," ~ case_item).++ ~? case_type ~ "with" ~? branches ~ "end" ^^ {
      (_:List[Line],
       cIs:((String,CaseExpr),Option[List[(String,CaseExpr)]]),
       oCt:Option[ConstrExpr],
      _:String,
       bs:Option[List[BranchExpr]],
      _:String) => {
        val cis = cIs._2 match {
          case Some(_cIs) => cIs._1._2 :: _cIs.map(_._2)
          case None => cIs._1._2 :: Nil
        }
        Cases(RegularStyle,oCt,cis,bs match {case Some(_bs) => _bs case None => Nil})
      }
    }

  lazy val case_item : Parser[CaseExpr] =
    oprConstr(100) ~? ("as" ~ primParse.name) ~? ("in" ~ pattern(200)) ^^ {
      (_,c,oSn,oSp) =>
        (c,oSn match {case Some(sN) => Some(sN._2) case None => None},
          oSp match {case Some(sP) => Some(sP._2) case None => None})
    }

  /**
    * References {{{return_type}}} Precedence of term is set to level 100,
    * oprConstr is not stated in the reference manual but in g_constr.mlg
    *
    * @see [[https://github.com/coq/coq/blob/master/parsing/g_constr.mlg]]
    */
  lazy val case_type:Parser[ConstrExpr] = "return" ~ oprConstr(100) ^^ {(_,_,c) => c}

  /**
    * References {{{dep_ret_type}}}
    */
  lazy val return_type : Parser[(Option[kernelTypes.name.T],Option[ConstrExpr])] =
    ("as" ~ primParse.name) ?~ case_type ^^ {
      (_,oNa,c) =>oNa match {case Some(n) => (Some(n._2),Some(c)) case None => (None,Some(c))}
    }

  lazy val branchIn: Parser[String] = "|" ^^ {(_,_) => "|"}


  lazy val branches : Parser[List[BranchExpr]] = branchIn ?~ eqn ~? ("|" ~ eqn).++ ^^ {
    (_,_,eqn1,ceqn1,eqns) =>
      eqns match {
        case Some(_eqns) => (eqn1,ceqn1) :: _eqns.map(_._2)
        case None => (eqn1,ceqn1) :: Nil
      }
  } | branchIn ^^ {(_,_) => Nil}

  lazy val mult_pattern : Parser[List[CasesPatternExpr]]= pattern(99) ~? ("," ~ pattern(99)).++ ^^ {
    (_,pt,ptss) =>
      ptss match {
        case Some (_ptss) =>  pt :: _ptss.map(pts => pts._2)
        case None => pt :: Nil
      }
  }

  lazy val eqn : Parser[BranchExpr]= mult_pattern ~? ("|" ~ mult_pattern).++ ~ "=>" ~ lconstr ^^ {
    (_,mpt,mptss,_,c) =>
      mptss match {
        case Some(_mptss) => (mpt :: _mptss.map(_._2),c )
        case None => (mpt :: Nil,c)
      }
  }

  lazy val impl_name_head : Parser[Name] = impl_ident_head ^^ {(_,id) => Name(id) }


  //TODO:binders can be empty with Nil as result
  lazy val binders : Parser[List[LocalBinderExpr]] = binder.++ ^^ {(_,bs) => bs.flatten}

  //TODO: Use meaningful exceptions
  lazy val impl_ident_head : Parser[id.T] = "{" ~ primParse.ident ^^
  {(_,_,id) => if (id.equals("wf") || id.equals("struct") || id.equals("measure"))
    {throw new IllegalStateException("Keyword not allowed")}
  else {
    id
  }}

  /**
    * Matches rule
    *
    * {{{
    *   sort         ::=  Prop | Set | Type
    * }}}
    *
    * except for rule '''"Type" ~ "@{" ~ universe ~ "}"'''. This is not stated in the reference manual but
    * int g_constr.mlg
    */
  lazy val sort:Parser[GlobSort] = (
    "Set" ^^ {(_,_) => pretypingTypes.globTerm.global.Set}
      | "Prop" ^^ {(_,_) => pretypingTypes.globTerm.global.Prop}
      | "Type" ^^ {(_,_) => pretypingTypes.globTerm.global.Type[SortInfo](Nil)}
      | "Type" ~ "@{" ~ universe ~ "}" ^^ {(_,_,_,u,_) => pretypingTypes.globTerm.global.Type[SortInfo](u)}
    )


  lazy val sort_family : Parser[kernelTypes.sorts.Family] = (
    "Set" ^^ {(_,_) => kernelTypes.sorts.InSet}
      | "Prop" ^^ {(_,_) => kernelTypes.sorts.InProp}
      | "Type" ^^ {(_,_) => kernelTypes.sorts.InType}
    )


  lazy val universe_expr : Parser[Option[(Qualid,Int)]] = (
    global ~ "+" ~ primParse.natural ^^ { (_, q, _, n) => Some(q,n) }
    | global ^^ {(_,q) => Some(q,0)}
    | "_" ^^ {(_,_) => None}
    )

  //TODO:Use meaningful exceptions
  lazy val universe : Parser[SortInfo] = (
    primParse.ident ~ "(" ~ universe_expr ~? ("," ~ universe_expr ).++ ~ ")"
      ^^ {(_,id,_,u,us,_) =>if(id.equals("max")){ u :: us.getOrElse(Nil).map(u => u._2)}else{throw new IllegalStateException("keyword not allowed here (must be max)")} }
    | universe_expr ^^ {(_,u) => List(u)}
    )

  lazy val constr : Parser[ConstrExpr]  = (
    oprConstr(8) ^^ {(_,c) => c}
    | "@" ~ global ~? instance ^^ {(_,_,g,i) => AppExpl((None,g,i match {case Some(_i) => _i case None => None}),Nil)}
    )


  lazy val record_fields : Parser[List[(Qualid,ConstrExpr)]]  = (
    record_field_declaration ~ ";" ~ record_fields ^^ {(_:List[Line],qc1:Qualid,qc2:ConstrExpr,_:String,rfs:List[(Qualid,ConstrExpr)]) => (qc1,qc2) :: rfs}
    | record_field_declaration ^^ {(_,q,c) => (q,c) :: Nil}
    )

  /**
    * References first 10 sub-rules of the term-rule
    *
    * In Detail:
    *
    * {{{forall binders , term
    * | fun binders => term
    * | fix fix_bodies
    * | cofix cofix_bodies
    * | let ident [binders] [: term] := term in term
    * | let fix fix_body in term
    * | let cofix cofix_body in term
    * | let ( [name , … , name] ) [dep_ret_type] := term in term
    * | let ' pattern [in term] := term [return_type] in term
    * | if term [dep_ret_type] then term else term}}}
    *
    * @note Sub-rules starting with '''let ' ''' differ strongly from the reference manual.
    *       '''case_type''' is only allowed in the specific case of a pattern with in. Also, the optional
    *       term denoted behind pattern is implemented as a pattern
    */
  lazy val binder_constr : Parser[ConstrExpr]  = (
    "forall" ~ open_binders ~ "," ~ oprConstr(200) ^^ {(_,_,ob,_,oc) => mkProdCN (ob)(oc)}
      | "fun" ~ open_binders ~ "=>" ~ oprConstr(200) ^^ {(_,_,ob,_,oc) => mkLambdaCN (ob)(oc)}
      | "let" ~ primParse.name ~? binders ~? opt_type_cstr ~ ":=" ~ oprConstr(200) ~
      "in" ~ oprConstr(200) ^^ {
      (_,tu,_,c1,_,c2) => {
        val id = tu._1._1._2
        val bl = tu._1._2
        val ty = tu._2
        //TODO: Discuss how tuple Notation (_,None), { CAst.v = CCast(c, CastConv t) } fits to returntype of type_cstr which should be Option[ConstrExpr] and not a tuple
        val tub = ty match {
          case None =>
            c1 match {
              case Cast(c,CastConv(t)) => (Some(t),c)
              case _ => (None,c1)
            }
          case _ => (ty.get,c1)
        }
        bl match {
          //TODO: In the original file, ty is a tuple, oprConstr probably leads back to AST construction
          case Some(_bl) => LetIn(id,mkLambdaCN(_bl)(tub._2),tub._1.map(mkProdCN(_bl)),c2)
          case None => LetIn(id,mkLambdaCN(Nil)(tub._2),tub._1.map(mkProdCN(Nil)),c2)
        }
      }
    }
      | "let" ~ single_fix ~ "in" ~ oprConstr(200) ^^ {
      (_:List[Line],_:String,fixb:Boolean,fixr:(id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr]),_:String,c:ConstrExpr) => {
        val fixp = mkSingleFix(fixb)(fixr)
        val id = fixp match {
          case Fix(id1,_) => id1
          case CoFix(id1,_) => id1
            //TODO: Throw meaningful excepetions
          case _ => throw new IllegalStateException("Should be Fix or CoFix")
        }
        LetIn(Name(id),fixp,None,c)
      }
    }
      | "let" ~ "(" ~? (primParse.name ~? ("," ~ primParse.name).++) ~ ")" ~? return_type ~
      ":=" ~ oprConstr(200) ~ "in" ~ oprConstr(200) ^^ {
      (_:List[Line],
        tu:((((String,String),Option[(kernelTypes.name.T,Option[List[(String,kernelTypes.name.T)]])]),String),Option[(Option[kernelTypes.name.T],Option[ConstrExpr])]),
        _:String,
        c1:ConstrExpr,
        _:String,
        c2:ConstrExpr) => {
        val names = tu._1._1._2 match {
          case Some(ns) => ns._1 :: ns._2.getOrElse(Nil).map(n => n._2)
          case None => Nil
        }
        val rt = tu._2
        rt match {
          case Some(_rt) => LetTuple (names, _rt, c1, c2)
          case None => LetTuple (names,(None,None),c1,c2)
        }
      }
    }
      | "let" ~ "'" ~ pattern(200) ~ ":=" ~ oprConstr(200) ~
      "in" ~ oprConstr(200) ^^ {
      (_:List[Line],
        tu:((String,String),CasesPatternExpr),
        _:String,
        c1:ConstrExpr,
        _:String,
        c2:ConstrExpr
      ) => {
        val pat = tu._2
        Cases(LetPatternStyle,None,List((c1,None,None)),List((List(List(pat)),c2)))
      }
    }
      | "let" ~ "'" ~ pattern(200) ~ ":=" ~ oprConstr(200) ~
      case_type ~ "in" ~ oprConstr(200) ^^ {
      (_:List[Line],
        tu:(((String,String),CasesPatternExpr),String),
        ct:ConstrExpr,
        c1:ConstrExpr,
        _:String,
        c2:ConstrExpr) => Cases(LetPatternStyle,Some(ct),List((c1,None,None)),List((List(List(tu._1._2)),c2)))
    }
      | "let" ~ "'" ~ pattern(200) ~ "in" ~ pattern(200) ~ ":=" ~
      oprConstr(200) ~ case_type ~ "in" ~ oprConstr(200) ^^ {
      (
        _:List[Line],
        tu:(((((String,String),CasesPatternExpr),String),CasesPatternExpr),String),
        ct:ConstrExpr,
        c1:ConstrExpr,
        _:String,
        c2:ConstrExpr
      ) => {
        val pat1 = tu._1._1._1._2
        val pat2 = tu._1._2
        Cases(LetPatternStyle,Some(ct),List((c1,aliasvar(pat1),Some(pat2))),List((List(List(pat1)),c2)))
      }
    }
      | "if" ~ oprConstr(200) ~? return_type ~ "then" ~
      oprConstr(200) ~ "else" ~ oprConstr(200) ^^ {
      (_:List[Line],
        tu:((String,ConstrExpr),Option[(Option[kernelTypes.name.T],Option[ConstrExpr])]),
        _:String,
        c1:ConstrExpr,
        _:String,
        c2:ConstrExpr) => {
        If(tu._1._2,tu._2 match {case Some(x) => x case None => (None,None)},c1,c2)
      }
    }
      | fix_constr
    )

  /**
    * References rule arg from reference manual:
    *
    * {{{
    *   arg          ::=  term
    *                   | ( ident := term )
    * }}}
    */
  lazy val appl_arg:Parser[(ConstrExpr,Option[Explication])]   = (
    "(" ~ primParse.ident ~ ":=" ~ lconstr ~ ")" ^^ { (_, _, i, _, c, _) => (c,Some(ExplByName(i)))}
    | oprConstr(9) ^^ {(_,c) => (c,None)}
    )

  /**
    * Atomic values for term. In Detail:
    *
    * {{{
    *     | qualid
    *     | sort
    *     | num
    *     | _
    * }}}
    *
    * Rules beginning with Questionmark are not referenced but used in g_constr.mlg
    */
  lazy val atomic_constr : Parser[ConstrExpr] = (
    global ~? instance ^^ {
      (_,g,i) => Ref(g,i match {case Some(_i) => _i case None => None})
    }
    | sort ^^ {
      (_,s) => Sort(s)
    }
    | primParse.int ^^ {
      (_,i) => Prim(Numeral(i,s = true))
    }
    | primParse.string ^^ {
      (_,s) => Prim(SString(s))
    }
    | "_" ^^ {
      (_,_) => Hole(None,IntroAnonymous,None)
    }
    | "?" ~ "[" ~ primParse.ident ~ "]" ^^ {
      (_,_,_,i,_) => Hole(None,IntroIdentifier(i),None)
    }
    | "?" ~ "[" ~ primParse.pattern_ident ~ "]" ^^ {
      (_,_,_,i,_) => Hole(None,IntroFresh(i),None)
    }
    | primParse.pattern_ident ~? evar_instance ^^ {
      (_,i,e) => {
        Evar(i,e match { case Some(x) => x.map(h => (h.id,h.c)) case None => Nil})
      }
    }
    )

  /**
    * unreferenced rule used in g_constr.mlg
    *
    * instanceHelper is used here, because implicit conversion of @{ in combination with ~? is not possible
    */
  lazy val instance:Parser[Option[InstanceExpr]] =
    instanceHelper ~? universe_level.++ ~ "}" ^^ { (_,_,l,_) => Some(l.getOrElse(Nil))}

  lazy val instanceHelper : Parser[String] = "@{" ^^ {(_,s) => s}

  lazy val universe_level : Parser[GlobLevel] = (
    "Set" ^^ {(_,_) => globTerm.global.Set}
    |"Prop" ^^ {(_,_) => globTerm.global.Prop}
    |  "Type" ^^ {(_,_) => globTerm.global.Type(globTerm.universe.Unknown)}
    |  "_" ^^ {(_,_) => globTerm.global.Type(globTerm.universe.Anonymous)}
    |  global ^^ {(_,id) => globTerm.global.Type(globTerm.universe.Named(id))}
    )

  /**
    * Defines Fix- and Cofix-rules in a combination of '''fix_bodies''' and '''cofix_bodies'''
    *
    * In Detail:
    * {{{
    *   term        ::= fix fix_bodies
    *                   | cofix cofix_bodies
    *   fix_bodies   ::=  fix_body
    *                   | fix_body with fix_body with … with fix_body for ident
    * cofix_bodies ::=  cofix_body
    *                   | cofix_body with cofix_body with … with cofix_body for ident
    * }}}
    */
  lazy val fix_constr : Parser[FixType] = (
    single_fix ^^ {
      (
        _:List[Line],
       fixb:Boolean,
       fixr:(id.T,List[LocalBinderExpr], Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr])
      ) => mkSingleFix(fixb)(fixr)
    }
    | single_fix ~ ("with" ~ fix_decl).++ ~ "for" ~ primParse.identref ^^
      {(_:List[Line],
        sfixb:Boolean,sfixr:(kernelTypes.id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr]),
        sdcls:List[(String,(kernelTypes.id.T,List[LocalBinderExpr],Option[RecursionOrderExpr],ConstrExpr,Option[ConstrExpr]))],
        _:String,
        id:kernelTypes.id.T) => mkFix(sfixb)(id)(sfixr :: sdcls.map(sdcl => sdcl._2))}
    )

  /**
    * Used for Fix and Cofix construction
    */
  lazy val fix_kw : Parser[Boolean] = (
    "fix" ^^ {(_,_) => true}
    |"cofix" ^^ {(_,_) => false}
    )

  lazy val record_pattern : Parser[(Qualid,CasesPatternExpr)] =
    global ~ ":=" ~ pattern(200) ^^ { (_,q,_,p) => (q,p)}


  lazy val record_patterns : Parser[List[(Qualid,CasesPatternExpr)]]= (
    record_pattern ~ ";" ~? record_patterns ^^ {
      (_:List[Line],
       rp1q:Qualid,
       rp1c:CasesPatternExpr,
       _:String,
        rps:Option[List[(Qualid,CasesPatternExpr)]]) => {
        rps match {
          case Some(_rps) => (rp1q,rp1c) :: _rps
          case None => (rp1q,rp1c) :: Nil
        }
      }
    }
    |record_pattern ~ ";" ^^ {(_:List[Line],rpq:Qualid,rpc:CasesPatternExpr,_:String) => List((rpq,rpc))}
    |record_pattern ^^ {(_,rpq,rpc) => List((rpq,rpc))}
    )

  lazy val impl_ident_tail : Parser[Name => LocalBinderExpr] = (
    "}" ^^ {(_,_) => HFuncs.binder_of_name(Implicit)(_)}
    |primParse.name.++ ~ ":" ~ lconstr ~ "}" ^^ { (_, nal, _, c, _) => (n:Name) => LocalAssum(n::nal,Default(Implicit),c)}
    |primParse.name.++ ~ "}" ^^ { (_, nal, _) => (n:Name) => LocalAssum(n::nal,Default(Implicit),Hole(Some(evarKinds.BinderType(n)),IntroAnonymous,None))}
    | ":" ~ lconstr ~ "}" ^^ {(_,_,c,_) => (n:Name) => LocalAssum(List(n),Default(Implicit),c)}
    )

  /**
    * References annotation rule from the reference manual. However,
    * sub-rules regarding wf and measure are not mentioned in the reference
    * manual
    */
  lazy val fixannot : Parser[RecursionOrderExpr] = (
    "{" ~  "struct" ~ primParse.identref ~ "}" ^^ { (_, _, _, id, _) => StructRec(id)}
    |"{" ~ "wf" ~ constr ~ primParse.identref ~ "}" ^^ { (_, _, _, c, id, _) => WfRec(id,c)}
    |"{" ~ "measure" ~ constr ~? primParse.identref ~? constr ~ "}" ^^ { (_, _, c, oId, oC, _) => MeasureRec(oId,c,oC)}
    )

  /**
    * binders [annotation] | binders | Epsilon
    */
    //TODO:potentially epsilon with (Nil,None) as result
  lazy val opt_binders_fixannot : Parser[(List[LocalBinderExpr],Option[RecursionOrderExpr])] = (
    impl_name_head ~ impl_ident_tail ~ opt_binders_fixannot ^^ {
      (_:List[Line],na:kernelTypes.name.Name,assum:kernelTypes.name.Name => LocalBinderExpr,bfbs:List[LocalBinderExpr],bfr:Option[RecursionOrderExpr]) =>
        (assum(na) :: bfbs,bfr)
    }
    |fixannot ^^ {(_:List[Line],fix:RecursionOrderExpr) => (Nil,Some(fix))}
    |binder ~ opt_binders_fixannot ^^ { (_:List[Line], b:List[LocalBinderExpr], bfixbs:List[LocalBinderExpr], bfixr:Option[RecursionOrderExpr]) =>
      (b ++ bfixbs,bfixr)
    }
    )

  /**
    * Representing Binders not parenthesized
    */
  lazy val open_binders : Parser[List[LocalBinderExpr]] = (
    primParse.name.++ ~ ":" ~ lconstr ^^ {
      (_,ids,_,c) => {
        List(LocalAssum(ids,Default(Explicit),c)).asInstanceOf[List[LocalBinderExpr]]
      }
    }
      //TODO: This is unambiguous because names can be parsed before applying binders rule or after applying binders rule, however same parse trees derive
      | primParse.name.++ ~? binders ^^ {
      (_,ids,bs) => {
        bs match {
          case Some(_bs) =>  HFuncs.binders_of_names(ids) ++ _bs
          case None =>  HFuncs.binders_of_names(ids)
        }
      }
    }
      | primParse.name ~ ".." ~ primParse.name ^^ {
      (_,n1,_,n2) => {
        List(LocalAssum(List(n1, kernelTypes.name.Name(".."), n2),Default(Explicit),Hole(None,IntroAnonymous,None)))
      }
    }
      | closed_binder ~? binders ^^ {(_,cBs,bs) =>
      bs match {
        case Some(_bs) => cBs ++ _bs
        case None => cBs
      }
    }
    )

  lazy val binder : Parser[List[LocalBinderExpr]] = (
    primParse.name ^^ { (_, id) => {
      List(LocalAssum(List(id),Default(Explicit),Hole(None,IntroAnonymous,None)))
      .asInstanceOf[List[LocalBinderExpr]]}
    }
    | closed_binder ^^ {(_,cl) => cl}
    )

  /**
    * Representing parenthesized binders
    */
  lazy val closed_binder : Parser[List[LocalBinderExpr]] = (
    "(" ~ primParse.name ~ primParse.name.++ ~ ":" ~ lconstr ~ ")" ^^ {
      (_,tu,ids,_,c,_) => {
      List(LocalAssum(tu._2 :: ids,Default(Explicit),c))
        .asInstanceOf[List[LocalBinderExpr]]
    }}
      | "(" ~ primParse.name ~ ":" ~ lconstr ~ ")" ^^ {
      (_,_,id,_,c,_) => {
        List(LocalAssum(List(id),Default(Explicit),c))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "(" ~ primParse.name ~ ":=" ~ lconstr ~ ")" ^^ {
      (_,_,id,_,c,_) => {
        c match {
          case Cast(c1, CastConv(t)) =>
            List(LocalDef(id,c1,Some(t)))
              .asInstanceOf[List[LocalBinderExpr]]
          case _ =>
            List(LocalDef(id,c,None))
              .asInstanceOf[List[LocalBinderExpr]]
        }
      }
    }
      | "(" ~ primParse.name ~ ":" ~ lconstr ~ ":=" ~ lconstr ~ ")" ^^ {
      (_,tr,t,_,c,_) => {
        List(LocalDef(tr._1._2,c,Some(t)))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "{" ~ primParse.name ~ "}" ^^ {
      (_,_,id,_) => {
        List(LocalAssum(List(id),Default(Implicit),Hole(None,IntroAnonymous,None)))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "{" ~ primParse.name ~ primParse.name.++ ~ ":" ~ lconstr ~ "}" ^^ {
      (_,tu,ids,_,c,_) => {
        List(LocalAssum(tu._2 :: ids,Default(Implicit),c))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "{" ~ primParse.name ~ ":" ~ lconstr ~ "}" ^^ {
      (_,_,id,_,c,_) => {
        List(LocalAssum(List(id),Default(Implicit),c))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "{" ~ primParse.name ~ primParse.name.++ ~ "}" ^^ {
      (_,_,id,ids,_) => {
        (id :: ids)
          .map(id => LocalAssum(List(id),Default(Implicit),Hole(None,IntroAnonymous,None)))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "`(" ~ typeclass_constraint ~? ("," ~ typeclass_constraint).++ ~ ")" ^^ {
      (_,_,tC,tCs,_) => {
        //List only containing typeclass constraints ignoring the Comma-String
        val tClConsts = tC :: tCs.getOrElse(Nil).map(tu => tu._2)
        tClConsts.map(hTCC => LocalAssum(List(hTCC.id), Generalized(Implicit,Explicit,hTCC.b),hTCC.c))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "`{" ~ typeclass_constraint ~? ("," ~ typeclass_constraint).++ ~ "}" ^^ {
      (_,_,tC,tCs,_) => {
        //List only containing typeclass constraints ignoring the Comma-String
        val tClConsts = tC :: tCs.getOrElse(Nil).map(tu => tu._2)
        tClConsts.map(hTCC => LocalAssum(List(hTCC.id), Generalized(Implicit,Implicit,hTCC.b),hTCC.c))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }
      | "'" ~ pattern(0) ^^ {
      (_,_,p) => {
        val tu : (CasesPatternExpr,Option[ConstrExpr]) = p match {
          case PatCast(p1,ty) => (p1,Some(ty))
          case _ => (p,None)
        }
        List(LocalPattern((tu._1,tu._2)))
          .asInstanceOf[List[LocalBinderExpr]]
      }
    }

  )

  lazy val typeclass_constraint : Parser[HTypes.HTypeClConst]= (
    "!" ~ oprConstr(200) ^^ {
      (_,_,c) => {
        HTypes.HTypeClConst(kernelTypes.name.Anonymous,b = true,c)
      }
    }
    | "{" ~ primParse.name ~ "}" ~ ":" ~? "!" ~ oprConstr(200) ^^ {
      (_,tu,_,_,oExc,c) => {
        oExc match {
          case Some(_) => HTypes.HTypeClConst(tu._2,b = true,c)
          case None => HTypes.HTypeClConst(tu._2,b = false,c)
        }
      }
    }
    | name_colon ~? "!" ~ oprConstr(200) ^^ {
      (_,nC,oExc,c) => {
        oExc match {
          case Some(_) => HTypes.HTypeClConst(nC,b = true,c)
          case None => HTypes.HTypeClConst(nC,b = false,c)
        }
      }
    }
    | oprConstr(200) ^^ {(_,c) => HTypes.HTypeClConst(kernelTypes.name.Anonymous,b = false,c)}
  )

  /**
    * Used for optional type annotation. Type-Annotation can be any term
    * on level 200
    */
    //TODO:potentially epsilon with None as result
  lazy val opt_type_cstr : Parser[Option[ConstrExpr]] =
    ":" ~ lconstr ^^ {(_,_,c) => Some(c)}


  lazy val name_colon : Parser[kernelTypes.name.T] = (
    primParse.ident ~ ":" ^^ {
      (_,id,_) => {
        kernelTypes.name.Name(id)
          .asInstanceOf[kernelTypes.name.T]
      }
    }
    | "_" ~ ":" ^^ {
      (_,_,_) => {
        kernelTypes.name.Anonymous
      }
    }
    )

  /**
    * Helper types for AST-construction. Not included in the final AST
    */
  object HTypes {
    sealed case class HTypeClConst(id:kernelTypes.name.T,
                                   b:Boolean,
                                   c:ConstrExpr)
    sealed case class HInst(id:kernelTypes.id.T, c:ConstrExpr)
    sealed case class HUniverseExpr(id:kernelTypes.id.T, i:Int)
  }

  object HFuncs {
    def binders_of_names(l:List[kernelTypes.name.T]):List[LocalBinderExpr] = {
      l.map(binder_of_name(Explicit))
    }

    def binder_of_name(expl:BindingKind)(name:kernelTypes.name.T):LocalBinderExpr = {
      LocalAssum(List(name),Default(expl),Hole(Some(evarKinds.BinderType(name)),IntroAnonymous,None))
    }
  }
}