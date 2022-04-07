trait Fraction {
  val num: Int
  val denom: Int

  def *(other: Fraction): Fraction
  def +(other: Fraction): Fraction
  def -(other: Fraction): Fraction
  def /(other: Fraction): Fraction
}


trait Loggable {
  def log(timeStamp: Long, msg: String) =
    println("[" + timeStamp.toString + "]: " + msg)
}


trait Simplifiable extends Fraction{
  def simplify(fraction: Fraction): Fraction = {
    var i = 2
    var num = fraction.num
    var denom = fraction.denom
    while (i <= num || i <= denom) {
      while (num % i == 0 && denom % i == 0) {
        num = num / i
        denom = denom / i
      }
      i += 1
    }
    Fraction(num, denom, false, true)
  }
}


object Fraction {
  def apply(num: Int, denom: Int, loggable: Boolean = false, simplifiable: Boolean = false): Fraction = {
    if (loggable) new LoggableImpl(num, denom)
    else if (simplifiable) new SimplifiableImpl(num, denom)
    else new DefaultImpl(num, denom)
  }

  private class DefaultImpl(val num: Int, val denom: Int) extends Fraction {
    override def *(other: Fraction): Fraction =
      Fraction(this.num * other.num, this.denom * other.denom)

    override def +(other: Fraction): Fraction =
      Fraction(this.num * other.denom + this.denom * other.num, this.denom * other.denom)

    override def -(other: Fraction): Fraction =
      Fraction(this.num * other.denom - this.denom * other.num, this.denom * other.denom)

    override def /(other: Fraction): Fraction =
      if (other.num != 0)
        Fraction(this.num * other.denom, this.denom * other.num)
      else
        Fraction(0, 0)

    override def toString() = num.toString + "/" + denom.toString
  }

  private class LoggableImpl(num: Int, denom: Int) extends DefaultImpl(num, denom) with Loggable {
    def timeStamp = System.nanoTime
    override def *(other: Fraction): Fraction = {
      log(timeStamp, "multiplying " + this.toString + " by " + other.toString)
      Fraction(this.num * other.num, this.denom * other.denom, true)
    }

    override def +(other: Fraction): Fraction = {
      log(timeStamp, "to " + this.toString + " adding " + other.toString)
      Fraction(this.num * other.denom + this.denom * other.num, this.denom * other.denom, true)
    }

    override def -(other: Fraction): Fraction = {
      log(timeStamp, "from " + this.toString + " subtracting " + other.toString)
      Fraction(this.num * other.denom - this.denom * other.num, this.denom * other.denom, true)
    }

    override def /(other: Fraction): Fraction = {
      log(timeStamp, "dividing " + this.toString + " by " + other.toString)
      if (other.num != 0)
        Fraction(this.num * other.denom, this.denom * other.num, true)
      else
        Fraction(0, 0, true)
    }
  }

  private class SimplifiableImpl(num: Int, denom: Int) extends DefaultImpl(num, denom) with Simplifiable {
    override def *(other: Fraction): Fraction = simplify(super.*(other))

    override def +(other: Fraction): Fraction = simplify(super.+(other))

    override def -(other: Fraction): Fraction = simplify(super.-(other))

    override def /(other: Fraction): Fraction = simplify(super./(other))
  }
}


object Appl {
  def main(args: Array[String]): Unit = {
    val f1 = Fraction(3, 7)
    val f2 = Fraction(4, 9)
    val f3 = Fraction(1, 19, true)
    val f1f2 = f1 * f2

    println(f1.toString + " * " + f2.toString + " = " + f1f2)
    println(f3.toString + " * " + f2.toString + " * " + f1.toString + " = " + f1 * f2 * f3)
    println(f1.toString + " + " + f2.toString + " = " + (f1 + f2))
    println(f1.toString + " - " + f2.toString + " = " + (f1 - f2))
    println(f1.toString + " / " + f2.toString + " = " + (f1 / f2))

    val f4 = Fraction(1, 3, false, true)
    val f5 = Fraction(1, 6, false, true)
    println(f4.toString + " * " + f5.toString + " = " + (f4 * f5))
    println(f4.toString + " + " + f5.toString + " = " + (f4 + f5))
    println(f4.toString + " - " + f5.toString + " = " + (f4 - f5))
    println(f4.toString + " / " + f5.toString + " = " + (f4 / f5))
  }
}