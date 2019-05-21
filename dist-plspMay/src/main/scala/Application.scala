/*
 * 作成したプログラムのアプリケーションを記述するためのファイル
 */

object plspSolver {
  def main(args: Array[String]): Unit = {

    val id: String = "199x223x" // 学籍番号を書く

    val fileName = args(0)

    println(s"ID: $id")
    println(s"CSP: $fileName")

    val csp = cspFactory.fromFile(fileName)

    println("c Parse Done.")

    val solver: CspSolver = new BackTrack // new "自分ソルバークラス" を書く
    val solution = solver.solve(csp)
    if (solution.nonEmpty) {
      println("s SAT")
      printAssignment(solution.get)
    } else {
      println("s UNSAT")
    }

  }

  def printAssignment(a: Assignment) = {
    a.amap.map { case (x, v) => s"v ${x.name} = $v" }.foreach(println)
  }
}

// file test
object FileTest extends App {
  val csp = cspFactory.fromFile("CspFiles/PLS05.csp")

  val solver = new BackTrack
  val solution = solver.solve(csp)

  if (solution.nonEmpty) {
    println("s SAT")
    println(solution.get)
  } else {
    println("s UNSAT")
  }
}

// ラテン方陣
object LatinSquare extends App {

  def allDiff(xs: Seq[Variable]) = {
    for(i <- 0 until xs.size; j <- i+1 until xs.size) yield Ne(xs(i), xs(j))
  }

  val x11 = Variable("x11")
  val x12 = Variable("x12")
  val x13 = Variable("x13")
  val x21 = Variable("x21")
  val x22 = Variable("x22")
  val x23 = Variable("x23")
  val x31 = Variable("x31")
  val x32 = Variable("x32")
  val x33 = Variable("x33")

  val dom = Domain(Seq(1, 2, 3))

  val c1x = allDiff(Seq(x11, x12, x13))
  val c2x = allDiff(Seq(x21, x22, x23))
  val c3x = allDiff(Seq(x31, x32, x33))
  val cx1 = allDiff(Seq(x11, x21, x31))
  val cx2 = allDiff(Seq(x12, x22, x32))
  val cx3 = allDiff(Seq(x13, x23, x33))

  val csp = CSP(
    Seq(x11, x12, x13,
        x21, x22, x23,
        x31, x32, x33),
    Map(x11 -> dom, x12 -> dom, x13 -> dom,
        x21 -> dom, x22 -> dom, x23 -> dom,
        x31 -> dom, x32 -> dom, x33 -> dom),
    c1x ++ c2x ++ c3x ++ cx1 ++ cx2 ++ cx3
  )

  csp.toSugar.foreach(println)
}
