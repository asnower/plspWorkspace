
/*
 * 制約充足問題 (Constraint Satisfaction Problem; CSP) に関するクラスを定義するためのファイル
 */

abstract class Expression

abstract class Term extends Expression {
  def vars: Set[Variable]
  def valuedWith(a: Assignment): Int
}

case class Variable(name: String) extends Term {
  def vars = Set(this)
  def valuedWith(a: Assignment) = a(this)
}

case class Domain(values: Seq[Int]) extends Expression {
  def lb = values.min
  def ub = values.max
  def size = values.size
}

abstract class Constraint extends Expression {
  def vars: Set[Variable]
  def isSatisfiedWith(a: Assignment): Boolean
}

abstract class LogicalFormula extends  Constraint {
  def vars: Set[Variable]
  def isSatisfiedWith(a: Assignment): Boolean
}

case class CSP(
                var vars: Seq[Variable],
                var doms: Map[Variable, Domain],
                var cons: Seq[Constraint]
              )  {

  def hasNoEmptyDomain = doms.forall(_._2.size > 0)
  def isSatisfiedWith(a: Assignment) = cons.forall(_.isSatisfiedWith(a))

  lazy val var2cons = (for (x <- vars) yield x -> cons.filter(_.vars.contains(x))).toMap

  def toSugar(t: Expression): String = t match {
      case x: Variable => s"(int ${x.name} ${toSugar(doms(x))})"
      case d: Domain => if (d.values.size == d.ub - d.lb + 1) s"${d.lb} ${d.ub}" else d.values.mkString(" ")
      // case c: Ne => s"(ne ${c.x1.name} ${c.x2.name})"
      // case Ne(x1: Term, x2: Term) => s"(ne ${x1.name} ${x2.name})"
      case Ne(x1: Term, x2: Term) => s"ne $x1 $x2"
      case Eq(x1: Term, x2: Term) => s"eq $x2 $x2"
  }

  def toSugar: Seq[String] = {
    vars.map(toSugar(_)) ++ cons.map(toSugar(_))
  }
}

object CSP {
  def apply() = new CSP(Seq.empty, Map.empty, Seq.empty)
}

case class Assignment(amap: Map[Variable, Int]) {
  def apply(x: Variable) = amap(x)
  //def apply(): Assignment = Assignment(Map.empty)

  def contains(x: Variable) = amap.contains(x)

  def +(x: Variable, v: Int) = Assignment(amap + (x -> v))

  def +(xv: Tuple2[Variable, Int]) = Assignment(amap + (xv._1 -> xv._2))

  def toDoms: Map[Variable, Domain] = amap.map { xv => xv._1 -> Domain(Seq(xv._2)) }

  override def toString = {
    amap.map{case (x, v) => s"v ${x.name} = $v"}.mkString("\n")
  }
}

object Assignment {
  def apply(): Assignment = Assignment(Map.empty)
}


/* -------------------- Term -------------------- */
/* num */
case class Num(n: Int) extends Term {
  override def vars: Set[Variable] = Set()
  override def valuedWith(a: Assignment): Int = n

}

/* (abs Term) */
case class Abs(t: Term) extends Term {
  override def vars: Set[Variable] = t.vars
  override def valuedWith(a: Assignment): Int = t.valuedWith(a).abs
}

/* (neg Term) */
case class Neg(t: Term) extends Term {
  override def vars: Set[Variable] = t.vars
  override def valuedWith(a: Assignment): Int = -t.valuedWith(a)
}

/* (add Term*) */
case class Add(ts: Seq[Term]) extends Term {
  override def vars: Set[Variable] = {
    var unionTs: Set[Variable] = Set()
    for (t <- ts) {
      unionTs = unionTs ++ t.vars
    }
    return unionTs
  }

  override def valuedWith(a: Assignment): Int = {
    var sumTs = 0
    for (t <- ts) {
      sumTs += t.valuedWith(a)
    }
    return sumTs
  }
}

/* (sub Term Term+) */
case class Sub(t1: Term, ts:Seq[Term]) extends Term {
  override def vars: Set[Variable] = {
    var unionTs: Set[Variable] = Set()
    for (t <- ts) {
      unionTs = unionTs ++ t.vars
    }
    return unionTs
  }

  override def valuedWith(a: Assignment): Int = {
    var sumTs = 0
    for (t <- ts) {
      sumTs += t.valuedWith(a)
    }
    return t1.valuedWith(a) - sumTs
  }
}

/* (mul Term Term) */
case class Mul(t1: Term, t2: Term) extends Term {
  override def vars: Set[Variable] = t1.vars ++ t2.vars
  override def valuedWith(a: Assignment): Int = t1.valuedWith(a) * t2.valuedWith(a)
}

/* (div Term Term) */
case class Div(t1: Term, t2: Term) extends Term {
  override def vars: Set[Variable] = t1.vars ++ t2.vars
  override def valuedWith(a: Assignment): Int = t1.valuedWith(a) / t2.valuedWith(a)
}

/* (mod Term Term) */
case class Mod(t1: Term, t2: Term) extends Term {
  override def vars: Set[Variable] = t1.vars ++ t2.vars
  override def valuedWith(a: Assignment): Int = t1.valuedWith(a) % t2.valuedWith(a)
}

/* (pow Term Term) */
case class Pow(t1: Term, t2: Term) extends Term {
  override def vars: Set[Variable] = t1.vars ++ t2.vars
  override def valuedWith(a: Assignment): Int = Math.pow(t1.valuedWith(a), t2.valuedWith(a)).toInt
}

/* (min Term*) */
case class Min(ts: Seq[Term]) extends Term {
  override def vars: Set[Variable] = {
    var unionTs: Set[Variable] = Set()
    for (t <- ts) {
      unionTs = unionTs ++ t.vars
    }
    return unionTs
  }
  override def valuedWith(a: Assignment): Int = {
    var minT = Int.MaxValue
    for (t <- ts) {
      if (minT > t.valuedWith(a)) {
        minT = t.valuedWith(a)
      }
    }
    return minT
  }
}

/* (max Term*) */
case class Max(ts: Seq[Term]) extends Term {
  override def vars: Set[Variable] = {
    var unionTs: Set[Variable] = Set()
    for (t <- ts) {
      unionTs = unionTs ++ t.vars
    }
    return unionTs
  }
  override def valuedWith(a: Assignment): Int = {
    var maxT = Int.MinValue
    for (t <- ts) {
      if (maxT < t.valuedWith(a)) {
        maxT = t.valuedWith(a)
      }
    }
    return maxT
  }
}

/* -------------------- Constraint -------------------- */
/* (not c) */
case class Not(c: Constraint) extends Constraint {
  override def vars: Set[Variable] = Set()
  override def isSatisfiedWith(a: Assignment): Boolean = {
    if (c.isSatisfiedWith(a)) {
      return false
    }
    return true
  }
}
/* (or c*) */
case class Or(cs: Seq[Constraint]) extends  Constraint {
  override def vars: Set[Variable] = Set()
  override def isSatisfiedWith(a: Assignment): Boolean = {
    for (c <- cs) {
      if (c.isSatisfiedWith(a)) {
        return true
      }
    }
    return false
  }
}
/* (and c*) */
case class And(cs: Seq[Constraint]) extends  Constraint {
  override def vars: Set[Variable] = Set()
  override def isSatisfiedWith(a: Assignment): Boolean = {
    for (c <- cs) {
      if (!(c.isSatisfiedWith(a))) {
        return false
      }
    }
    return true
  }
}
/* (imp c1 c2) */
case class Imp(c1: Constraint, c2: Constraint) extends Constraint {
  override def vars: Set[Variable] = Set()
  override def isSatisfiedWith(a: Assignment): Boolean = {
    if ((c1.isSatisfiedWith(a)) && !(c2.isSatisfiedWith(a))) {
      return false
    }
    return true
  }
}
/* (xor c1 c2) */
case class Xor(c1: Constraint, c2: Constraint) extends Constraint {
  override def vars: Set[Variable] = Set()
  override def isSatisfiedWith(a: Assignment): Boolean = {
    if (c1.isSatisfiedWith(a) && !(c2.isSatisfiedWith(a))) {
      return true
    }
    if (!(c1.isSatisfiedWith(a)) && c2.isSatisfiedWith(a)) {
      return true
    }
    return false
  }
}
/* (iff c1 c2) */
case class Iff(c1: Constraint, c2: Constraint) extends Constraint {
  override def vars: Set[Variable] = Set()
  override def isSatisfiedWith(a: Assignment): Boolean = {
    if (c1.isSatisfiedWith(a) && !(c2.isSatisfiedWith(a))) {
      return false
    }
    if (!(c1.isSatisfiedWith(a)) && c2.isSatisfiedWith(a)) {
      return false
    }
    return true
  }
}
/* (eq Term Term) */
case class Eq(t1: Term, t2: Term) extends Constraint {
  override def vars: Set[Variable] = t1.vars ++ t2.vars
  override def isSatisfiedWith(a: Assignment): Boolean = t1.valuedWith(a) == t2.valuedWith(a)
}
/* (ne Term Term) */
case class Ne(t1: Term, t2: Term) extends Constraint {
  override def vars: Set[Variable] = t1.vars ++ t2.vars
  override def isSatisfiedWith(a: Assignment): Boolean = t1.valuedWith(a) != t2.valuedWith(a)
}
case class Le(t1: Term, t2: Term) extends Constraint {
  override def vars: Set[Variable] = t1.vars ++ t2.vars
  override def isSatisfiedWith(a: Assignment): Boolean = t1.valuedWith(a) <= t2.valuedWith(a)
}
/* (lt Term Term) */
case class Lt(t1: Term, t2: Term) extends Constraint {
  override def vars: Set[Variable] = t1.vars ++ t2.vars
  override def isSatisfiedWith(a: Assignment): Boolean = t1.valuedWith(a) < t2.valuedWith(a)
}
/* (ge Term Term) */
case class Ge(t1: Term, t2: Term) extends Constraint {
  override def vars: Set[Variable] = t1.vars ++ t2.vars
  override def isSatisfiedWith(a: Assignment): Boolean = t1.valuedWith(a) >= t2.valuedWith(a)
}
/* (gt Term Term) */
case class Gt(t1: Term, t2: Term) extends Constraint {
  override def vars: Set[Variable] = t1.vars ++ t2.vars
  override def isSatisfiedWith(a: Assignment): Boolean = t1.valuedWith(a) > t2.valuedWith(a)
}
/* (alldifferent Term*) */
case class AllDiff(ts: Seq[Term]) extends Constraint {
  override def vars: Set[Variable] = {
    var unionTs: Set[Variable] = Set()
    for (t <- ts) {
      unionTs = unionTs ++ t.vars
    }
    return unionTs
  }
  override def isSatisfiedWith(a: Assignment): Boolean = {
    for (i <- 0 until ts.size) {
      for (j <- i+1 until ts.size) {
        if (ts(i).valuedWith(a) == ts(j).valuedWith(a)) {
          return false
        }
      }
    }
    return true
  }
}

/* -------------------- Factory Method -------------------- */
object cspFactory {
  private[this] def varFactory(x: SIntVar): Variable = Variable(x.name)
  private[this] def domFactory(d: SDomain) = {
    val ds = d.dom.foldLeft(Seq.empty[Int])((seq, lu) => seq ++ (lu._1 to lu._2))
    Domain(ds)
  }
  private[this] def termFactory(t: SugarCspTerm): Term = {
    t match {
        case n: SNum => Num(n.n)
        case x: SIntVar => varFactory(x)
        case SNeg(t: SugarCspTerm) => Neg(termFactory(t))
        case SAdd(ts: Seq[SugarCspTerm]) => Add(ts.map(termFactory(_)))
        case SSub(ts: Seq[SugarCspTerm]) => Sub(termFactory(ts.head), ts.tail.map(termFactory(_)))
        case SMul(t1: SugarCspTerm, t2:SugarCspTerm) => Mul(termFactory(t1), termFactory(t2))
        case SDiv(t1: SugarCspTerm, t2:SugarCspTerm) => Div(termFactory(t1), termFactory(t2))
        case SMod(t1: SugarCspTerm, t2:SugarCspTerm) => Mod(termFactory(t1), termFactory(t2))
        case SPow(t1: SugarCspTerm, t2:SugarCspTerm) => Pow(termFactory(t1), termFactory(t2))
        case SMin(ts: Seq[SugarCspTerm]) => Min(ts.map(termFactory(_)))
        case SMax(ts: Seq[SugarCspTerm]) => Max(ts.map(termFactory(_)))
    }
  }
  private[this] def constraintFactory(c: SugarCspConstraint): Constraint = {
    c match {
      case SNot(c: SugarCspConstraint) => Not(constraintFactory(c))
      case SOr(cs: Seq[SugarCspConstraint]) => Or(cs.map(constraintFactory(_)))
      case SAnd(cs: Seq[SugarCspConstraint]) => And(cs.map(constraintFactory(_)))
      case SImp(c1: SugarCspConstraint, c2: SugarCspConstraint) => Imp(constraintFactory(c1), constraintFactory(c2))
      case SXor(c1: SugarCspConstraint, c2: SugarCspConstraint) => Xor(constraintFactory(c1), constraintFactory(c2))
      case SIff(c1: SugarCspConstraint, c2: SugarCspConstraint) => Iff(constraintFactory(c1), constraintFactory(c2))
      case SEq(t1: SugarCspTerm, t2: SugarCspTerm) => Eq(termFactory(t1), termFactory(t2))
      case SNe(t1: SugarCspTerm, t2: SugarCspTerm) => Ne(termFactory(t1), termFactory(t2))
      case SLe(t1: SugarCspTerm, t2: SugarCspTerm) => Le(termFactory(t1), termFactory(t2))
      case SLt(t1: SugarCspTerm, t2: SugarCspTerm) => Lt(termFactory(t1), termFactory(t2))
      case SGe(t1: SugarCspTerm, t2: SugarCspTerm) => Ge(termFactory(t1), termFactory(t2))
      case SGt(t1: SugarCspTerm, t2: SugarCspTerm) => Gt(termFactory(t1), termFactory(t2))
      case SAllDiff(ts: Seq[SugarCspTerm]) => AllDiff(ts.map(termFactory(_)))
    }
  }
  def fromFile(fileName: String): CSP = {
    val csp = CSP()
    val sp = new SugarCspLangParser
    sp.parse(new java.io.File(fileName))
    sp.domMap.keys.foreach { x0 =>
      val x = varFactory(x0)
      csp.vars = x +: csp.vars
      csp.doms += x -> domFactory(sp.domMap(x0))
    }
    csp.cons = sp.scons.map(constraintFactory)
    return csp
  }
}