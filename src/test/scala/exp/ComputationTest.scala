package exp

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
}
