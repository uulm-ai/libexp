package exp.computation

import cats.Eval
import org.specs2.mutable.Specification
import exp.all._
import exp.node

class SimpleTabledEvaluatorTest extends Specification {
  case class SetChannel(name: String, cg: CGraph, columns: Set[String]) extends UnsafeChannel {
    var state: List[Valuation] = List()
    /** The nodes for which values must be provided to `eval`. */
    override def nodes: Set[CNode] = cg.reports.filter(r => columns(r.name)).map(_.node)(collection.breakOut)
    /** A `Valuation` that is valid for at least the nodes in `nodes`. */
    override def evalUnsafe(values: Valuation): Eval[Unit] = Eval.now{
      state = values :: state
    }
    def result: Seq[Map[String,Any]] = state
      .reverse
      .map(v => columns.map(s => s -> v(cg.reports.find(_.name == s).get.node)).toMap)
  }

  "single node" >> {
    val n = pure(List(1,2), "input").lift(2).addColumn("i")
    val cg: CGraph = node.buildCGraph(n, _ => null)
    val channel = SetChannel("test", cg, Set("i"))
    SimpleTabledEvaluator.evalReportsUnsafe(cg, Set(channel)).value
    channel.result === Seq(Map("i" -> 1),Map("i" -> 2))
  }
  "two nodes" >> {
    val n = pure(List(1,2), "input").lift(2).addColumn("i")
    val n2 = pure(List(3,4), "input2").lift(2).addColumn("i2")
    val tupled = ^(n, n2, "tupled")((_, _)).addColumn("t")
    val cg: CGraph = node.buildCGraph(tupled, _ => null)
    val channel1 = SetChannel("c1", cg, Set("i"))
    val channel2 = SetChannel("c2", cg, Set("i2"))
    val channel3 = SetChannel("t", cg, Set("t"))
    SimpleTabledEvaluator.evalReportsUnsafe(cg, Set(channel1,channel2,channel3)).value
    "only first input" >> (channel1.result === Seq(Map("i" -> 1), Map("i" -> 2)))
    "only second input" >> (channel2.result === Seq(Map("i2" -> 3), Map("i2" -> 4)))
    "both" >> (channel3.result.toSet === Set((1,3),(2,3),(1,4),(2,4)).map(t => Map("t" -> t)))

  }
}
