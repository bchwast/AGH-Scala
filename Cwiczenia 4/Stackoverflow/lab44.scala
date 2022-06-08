object Appl44 {
  def sumArrayRec2(elems: Array[Int]) = {
    def goFrom(i: Int, size: Int, elms: Array[Int]): Int = {
      if (i < size) elms(i) + goFrom(i + 1, size, elms)
      else 0
    }
    goFrom(0, elems.size, elems)
  }

  def recurseTest(i: Int, j: Int, arr: Array[Int]): Int = {
    try {
      recurseTest(i + 1, j + 1, arr)
    } catch { case e: java.lang.StackOverflowError =>
      println("Recursion depth on this system is " + i + ".")
      i
    }
  }

  def ackerFun(m: Int, n: Int): Int = {
    assert(m >= 0 && n >= 0)
    if (m == 0) n + 1
    else if (n == 0) ackerFun(m - 1, 1)
    else ackerFun(m - 1, ackerFun(m, n - 1))
  }

  def main(args: Array[String]) {
    val recDepth = recurseTest(0, 0, Array(1))
    println("sumArrayRec2 = " +
      sumArrayRec2((0 until recDepth).toArray))
  }
}