class Outer {
  class Inner {
    def m1In(arg: Outer) = arg
  }
  def m1Out(inArg: this.Inner) = inArg
  def m2Out(outArg: Outer#Inner) = outArg
}