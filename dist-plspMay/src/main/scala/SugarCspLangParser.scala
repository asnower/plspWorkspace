import scala.util.parsing.combinator._

abstract class SugarCspExpression

case class SDomain(dom: Seq[Tuple2[Int, Int]])

abstract class SugarCspTerm extends SugarCspExpression

case class SNum(n: Int) extends SugarCspTerm

case class SIntVar(name: String) extends SugarCspTerm

case class SAbs(t: SugarCspTerm) extends SugarCspTerm

case class SNeg(t: SugarCspTerm) extends SugarCspTerm

case class SAdd(ts: Seq[SugarCspTerm]) extends SugarCspTerm

case class SSub(ts: Seq[SugarCspTerm]) extends SugarCspTerm

case class SMul(t1: SugarCspTerm, t2: SugarCspTerm) extends SugarCspTerm

case class SDiv(t1: SugarCspTerm, t2: SugarCspTerm) extends SugarCspTerm

case class SMod(t1: SugarCspTerm, t2: SugarCspTerm) extends SugarCspTerm

case class SPow(t1: SugarCspTerm, t2: SugarCspTerm) extends SugarCspTerm

case class SMin(ts: Seq[SugarCspTerm]) extends SugarCspTerm

case class SMax(ts: Seq[SugarCspTerm]) extends SugarCspTerm

trait SugarCspConstraint extends SugarCspExpression

case class SNot(c: SugarCspConstraint) extends SugarCspConstraint

object STRUE extends SugarCspConstraint

object SFALSE extends SugarCspConstraint

case class SAnd(cs: Seq[SugarCspConstraint]) extends SugarCspConstraint

case class SOr(cs: Seq[SugarCspConstraint]) extends SugarCspConstraint

case class SImp(c1: SugarCspConstraint, c2: SugarCspConstraint) extends SugarCspConstraint

case class SXor(c1: SugarCspConstraint, c2: SugarCspConstraint) extends SugarCspConstraint

case class SIff(c1: SugarCspConstraint, c2: SugarCspConstraint) extends SugarCspConstraint

case class SBoolVar(name: String) extends SugarCspConstraint

case class SEq(t1: SugarCspTerm, t2: SugarCspTerm) extends SugarCspConstraint

case class SNe(t1: SugarCspTerm, t2: SugarCspTerm) extends SugarCspConstraint

case class SLe(t1: SugarCspTerm, t2: SugarCspTerm) extends SugarCspConstraint

case class SLt(t1: SugarCspTerm, t2: SugarCspTerm) extends SugarCspConstraint

case class SGe(t1: SugarCspTerm, t2: SugarCspTerm) extends SugarCspConstraint

case class SGt(t1: SugarCspTerm, t2: SugarCspTerm) extends SugarCspConstraint

case class SAllDiff(ts: Seq[SugarCspTerm]) extends SugarCspConstraint

class SugarCspLangParser extends JavaTokenParsers {
  var str2IntVar: Map[String, SIntVar] = Map.empty
  var str2BoolVar: Map[String, SBoolVar] = Map.empty
  var str2Domain: Map[String, SDomain] = Map.empty
  var domMap: Map[SIntVar, SDomain] = Map.empty
  var scons: IndexedSeq[SugarCspConstraint] = IndexedSeq.empty

  def parse(inputFile: java.io.File): Any = {
    val src = scala.io.Source.fromFile(inputFile)
    val empty = """\s*""".r

    for (line <- src.getLines() if !line.startsWith(";")) {
      line match {
        case empty() =>
        case _ => {
          val str = line.trim.split(';').head
          if (!parseAll(sugarCSP, str).toString.contains("parsed"))
            println(s"ERR: $line")
          // println(domMap)
        }
      }
    }
  }

  def sugarCSP: Parser[Any] =
    DomainDefinition |
      IntegerVariableDefinition |
      BooleanVariableDefinition |
      Constraint

  def buildDomain(lb: Int, ub: Int): SDomain = SDomain(Seq((lb, ub)))

  def buildDomain(ds: Seq[Tuple2[Int, Int]]): SDomain = SDomain(ds)

  def buildDomain(value: Int): SDomain = SDomain(Seq((value, value)))

  def buildIntegerVariable(varName: String, domName: String): SIntVar = {
    val x = SIntVar(varName)
    domMap += x -> str2Domain(varName)
    x
  }

  def buildIntegerVariable(varName: String, lb: Int, ub: Int): SIntVar = {
    val x = SIntVar(varName)
    domMap += x -> SDomain(Seq((lb, ub)))
    x
  }

  def buildIntegerVariable(varName: String, ds: Seq[Tuple2[Int, Int]]): SIntVar = {
    val x = SIntVar(varName)
    domMap += x -> SDomain(ds)
    x
  }

  def buildIntegerVariable(varName: String, value: Int): SIntVar = {
    val x = SIntVar(varName)
    domMap += x -> SDomain(Seq((value, value)))
    x
  }

  def buildBooleanVariable(name: String): SBoolVar = {
    SBoolVar(name)
  }

  def buildConstraint(c: SugarCspConstraint): SugarCspConstraint = {
    scons = scons :+ c
    c
  }

  /*
   * ============================================================
   * Domain Definition
   * ============================================================
   */
  /**
    * Integer ::= Digit+ | -Digit+
    * Digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
    */
  def Integer = wholeNumber ^^ { t => t.toInt }

  /**
    * Symbol ::= SymbolCharacter+  /* except sequences interpreted as integers */
    * SymbolCharacter ::= /* any character from A-Z a-z 0-9 _ . + - * / % = < > ! & | and \u00080-\u10FFFF */
    */
  def Symbol =
    """[A-Za-z_][-\u00080-\u10FFFF\+\*\/\%\=\<\>\!\&a-zA-Z0-9_\[\]]*""".r

  def Nil =
    """nil""".r

  /**
    * Comment ::=
    * ; /* followed by characters until the end of line */
    */
  // TODO

  /**
    * DomainDefinition ::=
    * (domain DomainName LowerBound UpperBound) |
    * (domain DomainName (Range+)) |
    * (domain DomainName Value)
    */
  def DomainDefinition =
    ("(domain" ~> DomainName) ~ LowerBound ~ (UpperBound <~ ")") ^^ {
      case name ~ lb ~ ub => str2Domain += name -> buildDomain(lb, ub)
    } |
      ("(domain" ~> DomainName) ~ (("(" ~> rep1(RangeDom)) <~ ")" <~ ")") ^^ {
        case name ~ rs => {
          str2Domain += name -> buildDomain(rs)
        }
      } |
      ("(domain" ~> DomainName) ~ (Value <~ ")") ^^ {
        case name ~ v => {
          str2Domain += name -> buildDomain(v)
        }
      }

  /**
    * DomainName ::= Symbol
    */
  def DomainName = Symbol

  /**
    * LowerBound ::= Integer
    * UpperBound ::= Integer
    */
  def LowerBound: Parser[Int] = Integer

  def UpperBound: Parser[Int] = Integer

  /** Value ::= Integer */
  def Value: Parser[Int] = Integer

  /**
    * Range ::= Integer | (Integer Integer)
    */
  def RangeDom: Parser[Tuple2[Int, Int]] =
    Integer ^^ { t => (t, t) } |
      ("(" ~> Integer) ~ (Integer <~ ")") ^^ {
        case lb ~ ub => (lb, ub)
      }


  /*
   * ============================================================
   * Integer Variable Definitions
   * ============================================================
   */
  /**
    * IntegerVariableDefinition ::=
    * (int IntegerVariableName DomainName) |
    * (int IntegerVariableName LowerBound UpperBound) |
    * (int IntegerVariableName (Range+)) |
    * (int IntegerVariableName Value)
    */
  def IntegerVariableDefinition =
    "(int" ~> IntegerVariableName ~ DomainName <~ ")" ^^ {
      case intname ~ domname => str2IntVar += intname -> buildIntegerVariable(intname, domname)
    } |
      "(int" ~> IntegerVariableName ~ LowerBound ~ UpperBound <~ ")" ^^ {
        case intname ~ lb ~ ub => str2IntVar += intname -> buildIntegerVariable(intname, lb, ub)
      } |
      ("(int" ~> IntegerVariableName) ~ ("(" ~> rep1(RangeDom) <~ ")" <~ ")") ^^ {
        case intname ~ rs => str2IntVar += intname -> buildIntegerVariable(intname, rs)
      } |
      "(int" ~> IntegerVariableName ~ Value <~ ")" ^^ {
        case intname ~ int => str2IntVar += intname -> buildIntegerVariable(intname, int)
      }

  /**
    * IntegerVariableName ::=
    * Symbol
    */
  def IntegerVariableName = Symbol

  /*
   * ============================================================
   * Boolean Variable Definitions
   * ============================================================
   */
  /**
    * BooleanVariableDefinition ::=
    * (bool BooleanVariableName)
    */
  def BooleanVariableDefinition =
    "(" ~> ("bool" ~> BooleanVariableName) <~ ")" ^^ {
      case boolname => str2BoolVar += boolname -> buildBooleanVariable(boolname)

    }

  /**
    * BooleanVariableName ::=
    * Symbol
    */
  def BooleanVariableName = Symbol

  /*
 * ============================================================
 * Term
 * ============================================================
 */

  /**
    * Term ::=
    * Integer |
    * IntegerVariableName |
    * (abs Term) |
    * (neg Term)       | (- Term) |
    * (add Term*)      | (+ Term*) |
    * (sub Term Term+) | (- Term Term+) |
    * (mul Term Term)  | (* Term Term) |
    * (div Term Term)  | (/ Term Term) |
    * (mod Term Term)  | (% Term Term) |
    * (pow Term Term)  |
    * (min Term Term)  |
    * (max Term Term)  |
    * (if LogicalFormula Term Term)
    */
  def SugarTerm: Parser[SugarCspTerm] =
    Integer ^^ {
      case integer => SNum(integer)
    } |
      IntegerVariableName ^^ {
        case intname => str2IntVar(intname)
      } |
      "(" ~> ("abs" ~ SugarTerm) <~ ")" ^^ {
        case op ~ term => SAbs(term) // makeScopTerm(op,Seq(term))
      } |
      "(" ~> ("neg" ~ SugarTerm) <~ ")" ^^ {
        case op ~ term => SNeg(term)
      } |
      "(" ~> ("add" ~ rep(SugarTerm)) <~ ")" ^^ {
        case op ~ terms => SAdd(terms)
      } |
      "(" ~> ("+" ~ rep(SugarTerm)) <~ ")" ^^ {
        case op ~ terms => SAdd(terms)
      } |
      "(" ~> ("sub" ~ SugarTerm ~ rep1(SugarTerm)) <~ ")" ^^ {
        case op ~ term ~ terms => SSub(Seq(term) ++ terms)
      } |
      "(" ~> ("-" ~ SugarTerm ~ rep1(SugarTerm)) <~ ")" ^^ {
        case op ~ term ~ terms => SSub(Seq(term) ++ terms)
      } |
      "(" ~> ("-" ~ SugarTerm) <~ ")" ^^ {
        case op ~ term => SNeg(term)
      } |
      "(" ~> ("mul" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ {
        case op ~ term1 ~ term2 => SMul(term1, term2)
      } |
      "(" ~> ("*" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ {
        case op ~ term1 ~ term2 => SMul(term1, term2)
      } |
      "(" ~> ("div" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ {
        case op ~ term1 ~ term2 => SDiv(term1, term2)
      } |
      "(" ~> ("/" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ {
        case op ~ term1 ~ term2 => SDiv(term1, term2)
      } |
      "(" ~> ("mod" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ {
        case op ~ term1 ~ term2 => SMod(term1, term2)
      } |
      "(" ~> ("%" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ {
        case op ~ term1 ~ term2 => SMod(term1, term2)
      } |
      "(" ~> ("pow" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ {
        case op ~ term1 ~ term2 => SPow(term1, term2)
      } |
      "(" ~> ("min" ~ SugarTerm ~ rep1(SugarTerm)) <~ ")" ^^ {
        case op ~ term ~ terms => SMin(Seq(term) ++ terms)
      } |
      "(" ~> ("max" ~ SugarTerm ~ rep1(SugarTerm)) <~ ")" ^^ {
        case op ~ term ~ terms => SMax(Seq(term) ++ terms)
      }

  /*|
       "(" ~> ("if" ~ LogicalFormula ~ SugarTerm ~ SugarTerm) <~ ")" ^^ {
         case op ~ c ~ t1 ~ t2 => If(c, t1, t2)
       }*/

  /*
   * ============================================================
   * Constraints
   * ============================================================
   */
  /**
    * Constraint ::=
    * LogicalFormula
    */
  def Constraint: Parser[SugarCspConstraint] = LogicalFormula ^^ {
    case c => buildConstraint(c: SugarCspConstraint)
  }

  /**
    * LogicalFormula ::=
    * AtomicFormula |
    * (not LogicalFormula) | (! LogicalFormula) |
    * (and LogicalFormula*) | (&& LogicalFormula*) |
    * (or LogicalFormula*) | (|| LogicalFormula*) |
    * (imp LogicalFormula LogicalFormula) | (=> LogicalFormula LogicalFormula) |
    * (xor LogicalFormula LogicalFormula) |
    * (iff LogicalFormula LogicalFormula)
    */
  def LogicalFormula: Parser[SugarCspConstraint] =
    AtomicFormula |
      "(" ~> ("not" ~ LogicalFormula) <~ ")" ^^ { case op ~ ctr => SNot(ctr) } |
      "(" ~> ("!" ~ LogicalFormula) <~ ")" ^^ { case op ~ ctr => SNot(ctr) } |
      "(" ~> ("and" ~ rep(LogicalFormula)) <~ ")" ^^ { case op ~ ctrs => if (ctrs.size == 0) STRUE else SAnd(ctrs) } |
      "(" ~> ("&&" ~ rep(LogicalFormula)) <~ ")" ^^ { case op ~ ctrs => if (ctrs.size == 0) STRUE else SAnd(ctrs) } |
      "(" ~> ("or" ~ rep(LogicalFormula)) <~ ")" ^^ { case op ~ ctrs => if (ctrs.size == 0) SFALSE else SOr(ctrs) } |
      "(" ~> ("||" ~ rep(LogicalFormula)) <~ ")" ^^ { case op ~ ctrs => if (ctrs.size == 0) SFALSE else SOr(ctrs) } |
      "(" ~> ("imp" ~ LogicalFormula ~ LogicalFormula) <~ ")" ^^ { case op ~ ctr1 ~ ctr2 => SImp(ctr1, ctr2) } |
      "(" ~> ("=>" ~ LogicalFormula ~ LogicalFormula) <~ ")" ^^ { case op ~ ctr1 ~ ctr2 => SImp(ctr1, ctr2) } |
      "(" ~> ("xor" ~ LogicalFormula ~ LogicalFormula) <~ ")" ^^ { case op ~ ctr1 ~ ctr2 => SXor(ctr1, ctr2) } |
      "(" ~> ("iff" ~ LogicalFormula ~ LogicalFormula) <~ ")" ^^ { case op ~ ctr1 ~ ctr2 => SIff(ctr1, ctr2) }

  /**
    * AtomicFormula ::=
    * false | true | BooleanVariableName |
    * (eq Term Term) | (= Term Term) |
    * (ne Term Term) | (!= Term Term) |
    * (le Term Term) | (<= Term Term) |
    * (lt Term Term) | (< Term Term) |
    * (ge Term Term) | (>= Term Term) |
    * (gt Term Term) | (> Term Term) |
    * (RelationName Term*) |
    * (PredicateName Term*) |
    * AllDifferentConstraint |
    * WeightedSumConstraint |
    * CumulativeConstraint |
    * ElementConstraint |
    * DisjunctiveConstraint |
    * Lex_lessConstraint |
    * Lex_lesseqConstraint |
    * NvalueConstraint |
    * Global_cardinalityConstraint |
    * Global_cardinality_with_costsConstraint |
    * CountConstraint
    */
  def AtomicFormula: Parser[SugarCspConstraint] =
    "false" ^^ {
      case _ => SFALSE
    } |
      "true" ^^ {
        case _ => STRUE
      } |
      BooleanVariableName ^^ {
        case bvar => str2BoolVar(bvar)
      } |
      "(" ~> ("eq" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SEq(term1, term2) } |
      "(" ~> ("=" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SEq(term1, term2) }  |
      "(" ~> ("ne" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SNe(term1, term2) } |
      "(" ~> ("!=" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SNe(term1, term2) } |
      "(" ~> ("le" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SLe(term1, term2) } |
      "(" ~> ("<=" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SLe(term1, term2) } |
      "(" ~> ("lt" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SLt(term1, term2) } |
      "(" ~> ("<" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SLt(term1, term2) }  |
      "(" ~> ("ge" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SGe(term1, term2) } |
      "(" ~> (">=" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SGe(term1, term2) } |
      "(" ~> ("gt" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SGt(term1, term2) } |
      "(" ~> (">" ~ SugarTerm ~ SugarTerm) <~ ")" ^^ { case op ~ term1 ~ term2 => SGt(term1, term2) } |
      "(" ~> ("alldifferent" ~  rep(SugarTerm)) <~ ")" ^^ { case op ~ terms => SAllDiff(terms) }
      //AllDifferentConstraint

  //def AllDifferentConstraint: Parser[SugarCspConstraint] = {
  //  "(" ~> ("alldifferent" ~  rep(SugarTerm)) <~ ")" ^^ { case op ~ terms => SAllDiff(terms) }
  //}
}

