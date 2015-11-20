package exp

import exp.Computation.CompTyper
import exp.stages.evaluation.Valuation
import org.specs2.mutable.Specification
import shapeless._

class ComputationTest extends Specification with ValMatchers {

  def evalClosed(nodes: Node[_]*): Val[Stream[Valuation]] = for{
    closed <- OpenQuery(nodes).close(Map())
    str <- SimpleStreamDriver.evalGraph(closed,0L)
  } yield str

  "simplest test" >>
    (evalClosed(Fixed("f",1 to 5)) must beSuccessfulWith(haveLength(5)))
  "simple lift test" >>
    (evalClosed(Fixed("fixed", IndexedSeq(Stream(1,2),Stream(3,4))).lift(2)) must beSuccessfulWith(haveLength(4)))
//  "cross product 2x2" >> {
//    val i1 = Fixed("i1", 1 to 3)
//    val i2 = Fixed("i2", 1 to 2)
//    val x = implicitly[(Fixed[Int],Fixed[Int]) <:< (Node[Int],Node[Int])]
//    val y = implicitly[(Fixed[Int] :: Fixed[Int] :: HNil) <:< (Node[Int] :: Node[Int] :: HNil)]
//    val c = Computation("paste")(i1 :: i2 :: HNil)((i1: Int, i2: Int) => i1 + i2)
//    (
//      CompTyper.shapelessProductInstance[
//        (Node[Int],Node[Int]),
//        Node[Int] :: Node[Int] :: HNil,
//        Int :: Int :: HNil,
//        Int,
//        (Int,Int) => Int
//        ])
//    evalClosed(c) must beSuccessfulWith(haveLength(6))
//  }
}
