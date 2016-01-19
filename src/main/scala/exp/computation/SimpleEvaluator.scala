package exp.computation

import com.typesafe.scalalogging.StrictLogging

class ProgressReporter(val totalSize: Long) extends StrictLogging {
  @volatile private var lastReport: Long = _
  @volatile private var amountDone: Long = 0L
  def reportAmount(doneItems: Long): Unit = {

  }
}

/** Non-Optimizing, single-threaded evaluator. */
object SimpleEvaluator {
  def evalStream(computation: CGraph): Stream[Valuation] = {

    def topoSort(remaining: Set[CEdge], acc: List[CEdge]): List[CEdge] = {
      val open = remaining.filterNot(r => r.ins.exists(remaining))
      if(open.isEmpty){
        require(remaining.isEmpty, "computation graph contains a directed cycle")
        acc.reverse
      } else {
        val n = open.head
        topoSort(remaining - n, n :: acc)
      }
    }

    val topoOrder: List[CEdge] = topoSort(computation.nodeClosure, Nil)

    def evaluate(n: CEdge, valuation: Valuation): Stream[Valuation] = n match {
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

/** Non-Optimizing, multi-threaded evaluator.
  * Works as follows:
  * 1. find a topological order for nodes
  * 2. finds a split-point within this order
  * 3. force the evaluation (outer product) for the first part of the order and parallelize over this collection
  **/
object SimpleParallelEvaluator extends StrictLogging {
  def evalStream(computation: CGraph): Seq[Valuation] = {

    def topoSort(remaining: Set[CEdge], acc: List[CEdge]): List[CEdge] = {

      val open = remaining.filterNot(r => r.ins.exists(remaining))
      if(open.isEmpty){
        require(remaining.isEmpty, "computation graph contains a directed cycle")
        acc.reverse
      } else {
        val n = open.head
        topoSort(remaining - n, n :: acc)
      }
    }

    val topoOrder: List[CEdge] = topoSort(computation.nodeClosure, Nil)

    val remainingCost = topoOrder.map{
      case CedgeND(_,_,_,_,cpuInit,cpuItem) => cpuInit + cpuItem
      case CedgeDet(_,_,_,cpu) => cpu
    }.scanRight(0d)(_ + _)

    val parTasks = topoOrder.map{
      case CedgeND(_,_,_,length,_,_) => length
      case cd: CedgeDet => 1d
    }.scanLeft(1d)(_ * _)

    logger.debug(s"remCost: $remainingCost\n parTasts: $parTasks")

    val parWork = (remainingCost zip parTasks).map{case (c,t) => c*t}

    val maxWork = parWork.max

    logger.debug(s"par Work curve: ${parWork.mkString(",")}, max is $maxWork")

    val (parEdges, seqEdges) = topoOrder.splitAt(parWork.indexOf(maxWork))

    logger.info(s"parallel over [${parEdges.map(_.name).mkString(",")}], sequential over [${seqEdges.map(_.name).mkString(",")}]")

    def evaluate(n: CEdge, valuation: Valuation): Stream[Valuation] = n match {
      case CedgeND(ins,_,f,_,_,_) =>
        f(ins.map(valuation.apply)).map(x => valuation + (n -> x))
      case CedgeDet(ins,_,f,_)    =>
        Stream(valuation + (n -> f(ins map valuation.apply)))
    }

    val sequentialValuations = parEdges.foldLeft(Stream(Valuation(Map()))){case (vs,node) =>
      vs.flatMap(v => evaluate(node,v))
    }

    sequentialValuations.par.flatMap(seqInitVal =>
      seqEdges.foldLeft(Stream(seqInitVal)){case (vs,node) =>
        vs.flatMap(v => evaluate(node,v))
      }.force
    ).seq
  }
}
