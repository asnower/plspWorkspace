/*
 * CSP ソルバーに関するクラスを定義するためのファイル
 */
abstract class CspSolver {
  def solve(csp: CSP): Option[Assignment]
}

class GenerateTest extends CspSolver {
  def solve(csp: CSP): Option[Assignment] = {
    def gt(xs: Seq[Variable], partialAssign: Assignment): Option[Assignment] = {

      if (xs.isEmpty) {
        print("Generate: ")
        println(partialAssign.amap.map{case (x,v) => s"${x.name} = $v"}.mkString(", "))
        print("Test: ")
        if (csp.isSatisfiedWith(partialAssign)) {
          println("OK")
          return Some(partialAssign)
        } else {
          println("NG")
          return None
        }
      }

      // xsがemptyでない時
      val x = xs.head
      for (v <- csp.doms(x).values) {
        println(s"${x.name} -> $v is added.")
        val sol = gt(xs.tail, partialAssign + (x -> v))
        if (sol.nonEmpty)
          return sol
      }
      return None
    }

    gt(csp.vars, Assignment())
  }
}

class BackTrack extends CspSolver {
  def solve(csp: CSP): Option[Assignment] = {

    def selectVariable(xs: Seq[Variable]): (Variable, Seq[Variable]) = (xs.head, xs.tail)
    // def selectVariable(xs: Seq[Variable]): (Variable, Seq[Variable]) = {
    //   var maxDomSizeX = 0
    //   var maxDomSizeI = 0
    //   var i = 0
    //   for (x <- xs) {
    //     if (maxDomSizeX < csp.doms(x).size){
    //       maxDomSizeI = i
    //       maxDomSizeX = csp.doms(x).size
    //     }
    //     i += 1
    //   }
    //   return (xs(maxDomSizeI), xs.filter(_ != xs(maxDomSizeI)))
    // }
    def valueOrder(dom: Domain): Seq[Int] = dom.values

    def bt(xs: Seq[Variable], partialAssign: Assignment): Option[Assignment] = {

      if (xs.isEmpty)
        return Some(partialAssign)

      val (x, remain) = selectVariable(xs)
      for (v <- valueOrder(csp.doms(x))) {
        val partial = partialAssign + (x -> v) // 変数 x に値 v を新たに割当てる
        /* CSP の制約の中で現在値が割当たっている変数上のもの (検査が可能な制約) */
        val consToBeTested = csp.cons.filter(c => c.vars.forall(partial.contains))
        /* 検査が通れば次の値割当てを行う */
        if (consToBeTested.forall(_.isSatisfiedWith(partial))) {
          val sol = bt(remain, partial)
          if (sol.nonEmpty)
            return sol
        }
      }
      return None
    }

    bt(csp.vars, Assignment())
  }
}
