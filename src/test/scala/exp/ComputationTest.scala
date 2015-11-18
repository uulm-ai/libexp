package exp

import fastparse.all._
import org.specs2.mutable.Specification

class ComputationTest extends Specification with ValMatchers {

  def evalClosed(nodes: Node[_]*): Val[Stream[Valuation]] = for{
    closed <- OpenQuery(nodes).close(Map())
    str <- Driver.evalGraph(closed,0L)
  } yield str

  "simplest test" >>
    (evalClosed(Fixed("f",1 to 5)) must beSuccessfulWith(haveLength(5)))
  "simple lift test" >>
    (evalClosed(Fixed("fixed", IndexedSeq(Stream(1,2),Stream(3,4))).lift(2)) must beSuccessfulWith(haveLength(4)))
  "cross product 2x2" >> {
    val i1: Node[Int] = Fixed("i1", 1 to 3)
    val i2: Node[Int] = Fixed("i2", 1 to 2)
    val c = Computation("paste")((i1,i2))((i1: Int, i2: Int) => i1 + i2)
    evalClosed(c) must beSuccessfulWith(haveLength(6))
  }

  "bug from test app" >> {
    val stringIn: Node[String] = FromString[String]("string", P(CharIn("abc").rep).!.map(s => Fixed("string", IndexedSeq(s))))
    val stringIn2: Node[Char] = FromString[String]("string2", P(CharIn("abcdefg").rep).!.map(s => Fixed("string2", IndexedSeq(s))))
      .map("stream2", (_:String).toStream, 0d).lift(10)
    val map = Computation("map", 1d)(stringIn){(string: String) => string.toStream}
    val lifted: Node[Char] = LiftND(map, 2)
    val mapChars = Computation("mapChars", 1d)(stringIn2){(c2: Char) => s"char x $c2"}

    val args = Array("--string","aa","--string2","cd")
    val r = for{
      withCli <- Driver.parseCli(OpenQuery(mapChars), args)
      result <- Driver.evalGraph(withCli, 0L)
    } yield result

    r must beSuccessfulWith(haveLength(4))
  }
}
