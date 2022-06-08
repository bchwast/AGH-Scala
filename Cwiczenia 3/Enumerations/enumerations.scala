object ThreeColors extends Enumeration {
  type ThreeColors = Value
  val Blue, White, Red = Value
}

import ThreeColors._

object utils {
  def leadingActor(part: ThreeColors) = part match {
  case Blue => "Juliette Binoche"
  case White => "Zbigniew Zamachowski"
  case Red => "Irene Jacob"
  }
}

object Appl {
  def main(args: Array[String]) {
    for (part <- ThreeColors.values)
      println(part.toString + ": " + utils.leadingActor(part))
  }
}