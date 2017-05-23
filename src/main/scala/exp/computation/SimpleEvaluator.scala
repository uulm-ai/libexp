package exp.computation

import com.typesafe.scalalogging.StrictLogging

/** Non-Optimizing, single-threaded evaluator. */
object SimpleEvaluator extends StrictLogging {
  def evalStream(computation: CGraph): Stream[Valuation] = {
    implicit val sortingOrder: Ordering[CNode] = Ordering.by(_.isInstanceOf[CedgeDet])
    val topoOrder: List[CNode] = exp.util.topoSort(computation.nodeClosure)
    logger.info("topological order used for computation: " + topoOrder.mkString(","))
    def evaluate(n: CNode, valuation: Valuation): Stream[Valuation] = n match {
      case CedgeND(ins,_,f,_,_,_) =>
        f(ins.map(valuation.apply)).map(x => valuation + (n -> x))
      case CedgeDet(ins,_,f,_)    =>
        Stream(valuation + (n -> f(ins map valuation.apply)))
    }

    topoOrder.foldLeft(Stream(Valuation(Map()))){case (vs,node) =>
        vs.flatMap(v => evaluate(node,v))
    }
  }
}
