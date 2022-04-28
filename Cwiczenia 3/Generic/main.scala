class SuperA1
class A1 extends SuperA1
class SubA1 extends A1

class GenInVar[T]
class GenCoVar[+T]
class GenContrVar[-T]

object Appl {
  def mInVarA1(inVar: GenInVar[A1]) {}
  def mCoVarA1(coVar: GenCoVar[A1]) {}
  def mContrVarA1(contrVar: GenContrVar[A1]) {}
  def main(args: Array[String]) {
//    mInVarA1(new GenInVar[SuperA1]) // (1)
    mInVarA1(new GenInVar[A1])
//    mInVarA1(new GenInVar[SubA1]) // (2)

//    mCoVarA1(new GenCoVar[SuperA1]) // (3)
    mCoVarA1(new GenCoVar[A1])

    // GenCoVar[SubA1] <: GenCoVar[A1]
    mCoVarA1(new GenCoVar[SubA1])

    // GenContrVar[SuperA1] <: GenContrVar[A1]
    mContrVarA1(new GenContrVar[SuperA1])
    mContrVarA1(new GenContrVar[A1])
//    mContrVarA1(new GenContrVar[SubA1]) // (4)
  }
}