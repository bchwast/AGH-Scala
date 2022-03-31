import scala.math._


class Int2DVec private (val x: Int, val y: Int) {
  def +(other: Int2DVec) = new Int2DVec(x + other.x, y + other.y)

  def -(other: Int2DVec) = new Int2DVec(x - other.x, y - other.y)

  def dot(other: Int2DVec) = x*other.x + y*other.y

  def unary_- = new Int2DVec(-x, -y)

  def |-?(other: Int2DVec): Boolean = (this dot other) == 0


  override def toString() = "(" + x.toString + ", " + y.toString + ")"
}


object Int2DVec {
  def apply(x: Int, y: Int) = new Int2DVec(x, y)
  def apply() = new Int2DVec(0, 0)
  def apply(prototype: Int2DVec) = new Int2DVec(prototype.x, prototype.y)

  def unitVectorOf(vec: Int2DVec) = {
    val length = sqrt(vec.x * vec.x + vec.y * vec.y)
    new Int2DVec((vec.x.toDouble / length).toInt, (vec.y.toDouble / length).toInt)
  }
}