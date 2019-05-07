
/*
 * 制約充足問題 (Constraint Satisfaction Problem; CSP) に関するクラスを定義するためのファイル
 */

abstract class Expression

abstract class Term extends Expression

case class Variable(name: String) extends Term

case class Domain(values: Seq[Int]) extends Expression {
  def lb = values.min
  def ub = values.max
  def size = values.size
}

abstract class Constraint extends Expression {
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
    case c: Ne => s"(ne ${c.x1.name} ${c.x2.name})"
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

  def contains(x: Variable) = amap.contains(x)

  def +(x: Variable, v: Int) = Assignment(amap + (x -> v))

  def +(xv: Tuple2[Variable, Int]) = Assignment(amap + (xv._1 -> xv._2))

  def toDoms: Map[Variable, Domain] = amap.map { xv => xv._1 -> Domain(Seq(xv._2)) }
}

/* x1とx2が異なる値をもつことを表す制約 */
case class Ne(x1: Variable, x2: Variable) extends Constraint {
  override def vars: Set[Variable] = Set(x1, x2)

  override def isSatisfiedWith(a: Assignment): Boolean = a(x1) != a(x2)
}


/* x1とx2が等しい値をもつことを表す制約 */
case class Eq(x1: Variable, x2: Variable) extends Constraint {
  override def vars: Set[Variable] = Set(x1, x2)

  override def isSatisfiedWith(a: Assignment): Boolean = a(x1) == a(x2)
}
