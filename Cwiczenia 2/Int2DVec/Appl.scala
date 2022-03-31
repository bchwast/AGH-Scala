object Appl {
  def main(args: Array[String]): Unit = {
    val v1 = Int2DVec(1, 2)
    val v2 = Int2DVec(10, 20)
    val v3 = v1 + v2
    val v4 = -v2
    val v5 = v1 - v2
    val dotty = v1 dot v2
    println(v3)
    println(v4)
    println(v5)
    println(dotty)

    println(Int2DVec())
    println(Int2DVec(v1 + v1 +  v2))
    println(Int2DVec.unitVectorOf(v2))

    println(Int2DVec(1, 0) |-? Int2DVec(0, 1))
    println(Int2DVec(1, 0) |-? Int2DVec(1, 1))
  }
}