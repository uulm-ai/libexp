package exp

import shapeless._

object Test {
  def main(args: Array[String]) {
    val stringIn: Node[String] = Provided[String]("string")(???)
    val doubleIn: Node[Double] = Provided[Double]("double")(???)
    val map = Computation("map", 1d)(stringIn :: HNil){(string: String) => string.toStream}
    val lifted: Node[Char] = LiftND(map, 2)
    val mapChars = Computation("mapChars", 1d)(lifted :: HNil){(c: Char) => s"char $c"}
  }
}