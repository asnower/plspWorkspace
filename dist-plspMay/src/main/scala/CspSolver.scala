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