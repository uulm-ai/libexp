package exp

import fastparse.all._
import shapeless._

/**
  * Created by thomas on 17.11.15.
  */
object Test {
  def main(args: Array[String]) {
    val stringIn: Node[String] = FromString[String]("string", P(CharIn("abc").rep).!.map(s => Fixed("string", IndexedSeq(s))))
    val stringIn2: Node[Char] = FromString[String]("string2", P(CharIn("abcdefg").rep).!.map(s => Fixed("string2", IndexedSeq(s))))
      .map("stream2", (_:String).toStream, 0d).lift(10)
    val map = Computation("map", 1d)(stringIn){(string: String) => string.toStream}
    val lifted: Node[Char] = LiftND(map, 2)
    val mapChars = Computation("mapChars", 1d)(stringIn2){(c2: Char) => s"char x $c2"}

    Driver.run(Set(mapChars), args)
  }
}
