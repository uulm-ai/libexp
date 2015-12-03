package exp.node

import exp.computation.{Valuation, SimpleEvaluator}
import org.specs2.mutable.Specification

/**
  * Created by thomas on 26.11.15.
  */
class SimpleEvaluator$Test extends Specification {
  import syntax._

  def eval(n: BaseNode[_]): Stream[Valuation] = {
    SimpleEvaluator.evalStream(Base.toCGraph(n))
  }

  "check length of evaluation result" >> {
    val n = fromSeq(1 to 5, "foo")
    eval(n) must haveLength(5)
  }

  "check length of cross product of two nodes" >> {
    val n = fromSeq(1 to 5, "foo")
    val m = fromSeq(1 to 3, "bar")
    eval(^(n,m)((_,_))) must haveLength(15)
  }

  "check length of lifted node" >> {
    val n = fromSeq(Seq(Stream(1,2),Stream(3,4)), "foo")
      .lift(2, "lift")
    eval(n) must haveLength(4)
  }
}
