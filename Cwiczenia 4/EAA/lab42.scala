object Appl42 {
  import scala.annotation.tailrec

  def checkPredicate(pred: Boolean, predAsString: String, prefix: String = "Checking if "): Unit = {
    if (pred) println(prefix + predAsString + ": OK")
    else println(prefix + predAsString + ": Fail")
  }

  def sumArrayIter(elems: Array[Int]): Int  = {
    var sum = 0
    for (i <- elems) sum += i
    sum
  }

  def sumArrayRec1(i: Int, elems: Array[Int]): Int = {
    if (i < elems.size) elems(i) + sumArrayRec1(i + 1, elems)
    else 0
  }

  def sumArrayRec2(elems: Array[Int]): Int = {
    val size = elems.size
    def goFrom(i: Int): Int = {
      if (i < size) elems(i) + goFrom(i + 1)
      else 0
    }
    goFrom(0)
  }

  def sumSqrArrayIter(elems: Array[Int]): Int  = {
    var sum = 0
    for (i <- elems) sum += i*i
    sum
  }

  def sumSqrArrayRec1(i: Int, elems: Array[Int]): Int = {
    if (i < elems.size) (elems(i) * elems(i)) + sumSqrArrayRec1(i + 1, elems)
    else 0
  }

  def sumSqrArrayRec2(elems: Array[Int]): Int = {
    val size = elems.size
    def goFrom(i: Int): Int = {
      if (i < size) (elems(i) * elems(i)) + goFrom(i + 1)
      else 0
    }
    goFrom(0)
  }

  def sumSqrArrayRec3(elems: Array[Int]): Int = {
    val size = elems.size
    @tailrec
    def goFrom(i: Int, acc: Int): Int = {
      if (i < size) goFrom(i + 1, acc + elems(i) * elems(i))
      else acc
    }
    goFrom(0, 0)
  }

  def prodArrayIter(elems: Array[Int]): Int  = {
    var prod = 1
    for (i <- elems) prod *= i
    prod
  }

  def prodArrayRec1(i: Int, elems: Array[Int]): Int = {
    if (i < elems.size) elems(i) * prodArrayRec1(i + 1, elems)
    else 1
  }

  def prodArrayRec2(elems: Array[Int]): Int = {
    val size = elems.size
    def goFrom(i: Int): Int = {
      if (i < size) elems(i) * goFrom(i + 1)
      else 1
    }
    goFrom(0)
  }

  def prodArrayRec3(elems: Array[Int]): Int = {
    val size = elems.size
    @tailrec
    def goFrom(i: Int, acc: Int): Int = {
      if (i < size) goFrom(i + 1, acc * elems(i))
      else acc
    }
    goFrom(0, 1)
  }

  def sumAbsArrayIter(elems: Array[Int]): Int  = {
    var sum = 0
    for (i <- elems) sum += i.abs
    sum
  }

  def sumAbsArrayRec1(i: Int, elems: Array[Int]): Int = {
    if (i < elems.size) elems(i).abs + sumAbsArrayRec1(i + 1, elems)
    else 0
  }

  def sumAbsArrayRec2(elems: Array[Int]): Int = {
    val size = elems.size
    def goFrom(i: Int): Int = {
      if (i < size) elems(i).abs + goFrom(i + 1)
      else 0
    }
    goFrom(0)
  }

  def sumAbsArrayRec3(elems: Array[Int]): Int = {
    val size = elems.size
    def goFrom(i: Int, acc: Int): Int = {
      if (i < size) goFrom(i + 1, acc + elems(i).abs)
      else acc
    }
    goFrom(0, 0)
  }

  def main(args: Array[String]): Unit = {
    val a1To5 = (1 to 5).toArray
    println("Testing sum with a1To5 = " + a1To5.mkString("Array(", ",", ") ..."))
    val expectedResultSum = 15
    checkPredicate(sumArrayIter(a1To5) == expectedResultSum, "sumArrayIter(0, a1To5) == " + expectedResultSum)
    checkPredicate(sumArrayRec1(0, a1To5) == expectedResultSum, "sumArrayRec1(0, a1To5) == " + expectedResultSum)
    checkPredicate(sumArrayRec2(a1To5) == expectedResultSum, "sumArrayRec2(0, a1To5) == " + expectedResultSum)

    println("Testing sumSqr with a1To5 = " + a1To5.mkString("Array(", ",", ") ..."))
    val expectedResultSumSqr = 55
    checkPredicate(sumSqrArrayIter(a1To5) == expectedResultSumSqr, "sumSqrArrayIter(0, a1To5) == " + expectedResultSumSqr)
    checkPredicate(sumSqrArrayRec1(0, a1To5) == expectedResultSumSqr, "sumSqrArrayRec1(0, a1To5) == " + expectedResultSumSqr)
    checkPredicate(sumSqrArrayRec2(a1To5) == expectedResultSumSqr, "sumSqrArrayRec1(0, a1To5) == " + expectedResultSumSqr)
    checkPredicate(sumSqrArrayRec3(a1To5) == expectedResultSumSqr, "sumSqrArrayRec3(0, a1To5) == " + expectedResultSumSqr)

    println("Testing prod with a1To5 = " + a1To5.mkString("Array(", ",", ") ..."))
    val expectedResultProd = 120
    checkPredicate(prodArrayIter(a1To5) == expectedResultProd, "prodArrayIter(0, a1To5) == " + expectedResultProd)
    checkPredicate(prodArrayRec1(1, a1To5) == expectedResultProd, "prodArrayRec1(0, a1To5) == " + expectedResultProd)
    checkPredicate(prodArrayRec2(a1To5) == expectedResultProd, "prodArrayRec2(0, a1To5) == " + expectedResultProd)
    checkPredicate(prodArrayRec3(a1To5) == expectedResultProd, "prodArrayRec3(0, a3To5) == " + expectedResultProd)


    println("Testing sumAbs with a1To5 = " + a1To5.mkString("Array(", ",", ") ..."))
    val expectedResultSumAbs = 15
    checkPredicate(sumAbsArrayIter(a1To5) == expectedResultSumAbs, "sumAbsArrayIter(0, a1To5) == " + expectedResultSumAbs)
    checkPredicate(sumAbsArrayRec1(0, a1To5) == expectedResultSumAbs, "sumAbsArrayRec1(0, a1To5) == " + expectedResultSumAbs)
    checkPredicate(sumAbsArrayRec2(a1To5) == expectedResultSumAbs, "sumAbsArrayRec2(0, a1To5) == " + expectedResultSumAbs)
    checkPredicate(sumAbsArrayRec3(a1To5) == expectedResultSumAbs, "sumAbsArrayRec3(0, a1To5) == " + expectedResultSumAbs)
  }
}