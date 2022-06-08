object Appl43 {
  def checkPredicate(pred: Boolean, predAsString: String, prefix: String = "Checking if "): Unit = {
    if (pred) println(prefix + predAsString + ": OK")
    else println(prefix + predAsString + ": Fail")
  }
  def maxIter(elems: Array[Int]) = {
    var max = Int.MinValue
    for (i <- elems) if (i > max) max = i
    max
  }
  def maxRec1(i: Int, maxEl: Int, elems: Array[Int]): Int = {
    if (i < elems.size) maxRec1(i + 1, math.max(elems(i), maxEl), elems)
    else maxEl
  }
  def maxRec2(elems: Array[Int]) = {
    def goFrom(i: Int, maxEl: Int, size: Int): Int = {
      if (i < size) goFrom(i + 1, math.max(elems(i), maxEl), size)
      else maxEl
    }
    goFrom(0, Int.MinValue, elems.size)
  }

  def minIter(elems: Array[Int]) = {
    var min = Int.MaxValue
    for (i <- elems) if (i < min) min = i
    min
  }
  def minRec1(i: Int, minEl: Int, elems: Array[Int]): Int = {
    if (i < elems.size) minRec1(i + 1, math.min(elems(i), minEl), elems)
    else minEl
  }
  def minRec2(elems: Array[Int]) = {
    def goFrom(i: Int, minEl: Int, size: Int): Int = {
      if (i < size) goFrom(i + 1, math.min(elems(i), minEl), size)
      else minEl
    }
    goFrom(0, Int.MaxValue, elems.size)
  }

  def main(args: Array[String]) {
    val arr1 = Array(3, 12, 43, 4, 10)
    println("Testing with arr1 = " +
      arr1.mkString("Array(", ", ", ") ..."))
    val expectResultMax = 43
    checkPredicate(maxIter(arr1) == expectResultMax,
      "maxIter(arr1) == " + expectResultMax)
    checkPredicate(maxRec1(0, Int.MinValue, arr1) == expectResultMax,
      "maxRec1(0, Int.MinValue, arr1) == " + expectResultMax)
    checkPredicate(maxRec2(arr1) == expectResultMax,
      "maxRec2(arr1) == " + expectResultMax)

    val expectResultMin = 3
    checkPredicate(minIter(arr1) == expectResultMin,
      "minIter(arr1) == " + expectResultMin)
    checkPredicate(minRec1(0, Int.MaxValue, arr1) == expectResultMin,
      "minRec1(0, Int.MaxValue, arr1) == " + expectResultMin)
    checkPredicate(minRec2(arr1) == expectResultMin,
      "minRec2(arr1) == " + expectResultMin)
  }
}