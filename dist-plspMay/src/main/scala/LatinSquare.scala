object Application extends App {

  def allDiff(xs: Seq[Variable]) = {
    for( i<- 0 until xs.size; j <- i+1 until xs.size) yield Ne(xs(i), xs(j))
  }

  val x1 = Variable("x1")
  val x2 = Variable("x2")
  val x3 = Variable("x3")

  val dom1 = Domain(Seq(1, 2, 3))
  val dom2 = Domain(Seq(1, 2, 3))

  val c = allDiff(Seq(x1, x2, x3))

  val csp = CSP(Seq(x1, x2), Map(x1 -> dom1, x2->dom2), c)

  csp.toSugar.foreach(println)
}