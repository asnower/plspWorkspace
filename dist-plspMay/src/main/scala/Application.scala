
/*
 * 作成したプログラムのアプリケーションを記述するためのファイル
 */

object Application extends App {

  def allDiff(xs: Seq[Variable]) = {
    for( i<- 0 until xs.size; j <- i+1 until xs.size) yield Ne(xs(i), xs(j))
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

  val dom11 = Domain(Seq(1, 2, 3))
  val dom12 = Domain(Seq(1, 2, 3))
  val dom13 = Domain(Seq(1, 2, 3))

  val dom21 = Domain(Seq(1, 2, 3))
  val dom22 = Domain(Seq(1, 2, 3))
  val dom23 = Domain(Seq(1, 2, 3))

  val dom31 = Domain(Seq(1, 2, 3))
  val dom32 = Domain(Seq(1, 2, 3))
  val dom33 = Domain(Seq(1, 2, 3))

  val c1 = allDiff(Seq(x11, x12, x13))
  val c2 = allDiff(Seq(x21, x22, x23))
  val c3 = allDiff(Seq(x31, x32, x33))

  val csp = CSP(
    Seq(x11, x12, x13, x21, x22, x23, x31, x32, x33),
    Map(x11 -> dom11, x12->dom12),
    c1
  )

  csp.toSugar.foreach(println)
}

object Test extends App {

  val csp = cspFactory.fromFile("CspFiles/PLS03.csp")

  println(csp)
}