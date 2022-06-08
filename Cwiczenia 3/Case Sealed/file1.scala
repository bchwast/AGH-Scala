sealed trait T1 {
  def f(i: Int): Unit
}
class C1 extends T1 {
  def f(i: Int) = { println(i) }
}