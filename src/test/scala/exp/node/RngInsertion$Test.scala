package exp.node

import exp.computation.{Valuation, SimpleEvaluator}
import org.specs2.mutable.Specification

import scala.util.Random

/**
  * Created by thomas on 26.11.15.
  */
class RngInsertion$Test extends Specification {
  import syntax._

  def eval[S <: Stage](seeds: Seq[Long], n: Node[S,_])(implicit cast: StageCast[S,RngInsertion.type]): Stream[Valuation] =
    SimpleEvaluator.evalStream(Base.toCGraph(RngInsertion.insertSeeds(seeds,n)))

  "create a random seed" >> {
    val n = seed("rand1")
      .map(new Random(_).nextGaussian(), name = "gaussian")

    eval(1L to 3L, n) must haveLength(3)
  }

  "compile-time: lift stage to rng insertion" >> {
    val n1: BaseNode[Int] = fromSeq(1 to 5, "n1")
    val s: RngNode[Long] = seed("foo")
    val x: RngNode[Long] = ^(s,n1)(_ + _)

    1 === 1
  }

}
