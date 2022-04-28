class SuperA1
class A1 extends SuperA1
class SubA1 extends A1

class GenInVar[T](private var arg: T) {
  def in(a: T) {}
  def inOut(a: T): T = a
  def out: T = arg
}

class GenCoVar[+T](private val arg: T) {
  //def in(a: T) {} // (1)
  def inOut[A >: T](a: A): A = a
  def out: T = arg
}

class GenContrVar[-T] {
  //(private val arg: T)*/  /(2)
  def in(a: T) {}
  //def inOut(a: T): T = a (3)
  //def out: T = // ?
}

object Appl {
  def main(args: Array[String]) {
    val genInVarA1 = new GenInVar[A1](new A1)
    val genCoVarA1 = new GenCoVar[A1](new A1)
    val genContrVarA1 = new GenContrVar[A1]

    //genInVarA1.in(new SuperA1) // (4)
    genInVarA1.in(new A1)
    genInVarA1.in(new SubA1)

    //genInVarA1.inOut(new SuperA1) // (5)
    genInVarA1.inOut(new A1)
    genInVarA1.inOut(new SubA1)

    val r1: SuperA1 = genInVarA1.out
    val r2: A1 = genInVarA1.out
    //val r3: SubA1 = genInVarA1.out // (6)

    val r4: SuperA1 = genCoVarA1.inOut(new SuperA1)
    //val r5: A1 = genCoVarA1.inOut(new SuperA1) // (7)
    //val r6: SubA1 = genCoVarA1.inOut(new SuperA1) // (8)
    val r7: SuperA1 = genCoVarA1.inOut(new A1)
    val r8: A1 = genCoVarA1.inOut(new A1)
    //val r9: SubA1 = genCoVarA1.inOut(new A1) // (9)
    val r10: SuperA1 = genCoVarA1.inOut(new SubA1)
    val r11: A1 = genCoVarA1.inOut(new SubA1)
    //val r12: SubA1 = genCoVarA1.inOut(new SubA1) // (10)
    val r12: SuperA1 = genCoVarA1.out
    val r13: A1 = genCoVarA1.out
    //val r14: SubA1 = genCoVarA1.out // (11)

    //genContrVarA1.in(new SuperA1) // (12)
    genContrVarA1.in(new A1)
    genContrVarA1.in(new SubA1)
  }
}
