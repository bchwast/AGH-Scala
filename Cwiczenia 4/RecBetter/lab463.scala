object Appl463 {
  import scala.util.control.TailCalls._

  private def isEven(n: Int): TailRec[Boolean] =
    if (n == 0) done(true) else tailcall(isOdd(n - 1))

  private def isOdd(n: Int): TailRec[Boolean] =
    if (n == 0) done(false) else tailcall(isEven(n - 1))

  def main(args: Array[String]) {
    val n = 99999
    val str = if (isEven(n).result) " is even." else " is odd."
    println(n.toString + str)
  }
}