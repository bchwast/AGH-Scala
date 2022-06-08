object Appl462 {
  def odd(n: Int): Boolean = {
    if (n == 0) false
    else even(n - 1) // indirect recursion
  }

  def even(n: Int): Boolean = {
    if (n == 0) true
    else odd(n - 1) // indirect recursion
  }

  def main(args: Array[String]) {
    val n = 999
    val str = if (even(n)) " is even." else " is odd."
    println(n.toString + str)
  }
}