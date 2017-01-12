package exp.computation

import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global

/** Non-Optimizing, single-threaded evaluator. */
object SimpleEvaluator extends StrictLogging {
  def evalStream(computation: CGraph): Stream[Valuation] = {

    def topoSort(remaining: Set[CNode], acc: List[CNode]): List[CNode] = {
      val open = remaining.filterNot(r => r.ins.exists(remaining))
      if(open.isEmpty){
        require(remaining.isEmpty, "computation graph contains a directed cycle")
        acc.reverse
      } else {
        val n = open.minBy(e => e.isInstanceOf[CedgeDet])
        topoSort(remaining - n, n :: acc)
      }
    }

    val topoOrder: List[CNode] = topoSort(computation.nodeClosure, Nil)
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

/** Non-Optimizing, multi-threaded evaluator.
  * Works as follows:
  * 1. find a topological order for nodes
  * 2. finds a split-point within this order
  * 3. force the evaluation (outer product) for the first part of the order and parallelize over this collection
  **/
object SimpleParallelEvaluator extends StrictLogging {
  def evalStream(computation: CGraph, desiredParallelism: Int = Runtime.getRuntime.availableProcessors * 2): Iterator[Valuation] = {

    def topoSort(remaining: Set[CNode], acc: List[CNode]): List[CNode] = {

      val open = remaining.filterNot(r => r.ins.exists(remaining))
      if(open.isEmpty){
        require(remaining.isEmpty, "computation graph contains a directed cycle")
        acc.reverse
      } else {
        val n = open.minBy(e => (
          e.isInstanceOf[CedgeND],
          Some(e).collect{case nd: CedgeND => nd.estimatedLength}.getOrElse(1d)
        ))
        topoSort(remaining - n, n :: acc)
      }
    }

    val topoOrder: List[CNode] = topoSort(computation.nodeClosure, Nil)

    logger.info("topological node order used for computation: " + topoOrder.map(_.name).mkString(","))

    val remainingCost = topoOrder.map{
      case CedgeND(_,_,_,_,cpuInit,cpuItem) => cpuInit + cpuItem
      case CedgeDet(_,_,_,cpu) => cpu
    }.scanRight(0d)(_ + _)

    val parTasks: Seq[Double] = topoOrder.map{
      case CedgeND(_,_,_,length,_,_) => length
      case cd: CedgeDet => 1d
    }.scanLeft(1d)(_ * _)

    logger.debug(s"remCost: $remainingCost\n parTasts: $parTasks")

    val parWork: Seq[Double] = (remainingCost zip parTasks).map{case (c,t) => c*t}

    val maxWork: Double = parWork.max

    logger.debug(s"par Work curve: ${parWork.mkString(",")}, max is $maxWork")

    val (parEdges: Seq[CNode], seqEdges: Seq[CNode]) = topoOrder.splitAt(parWork.indexOf(maxWork))

    logger.info(s"found parallelization scheme:\n\tparallel:\t${parEdges.map(_.name).mkString(",")}\n\tsequential:\t${seqEdges.map(_.name).mkString(",")}")

    def evaluate(n: CNode, valuation: Valuation): Stream[Valuation] = n match {
      case CedgeND(ins,_,f,_,_,_) =>
        f(ins.map(valuation.apply)).map(x => valuation + (n -> x))
      case CedgeDet(ins,_,f,_)    =>
        Stream(valuation + (n -> f(ins map valuation.apply)))
    }

    def parallelValuations: Stream[Valuation] =
      parEdges.foldLeft(Stream(Valuation(Map()))){case (vs,node) => vs.flatMap(v => evaluate(node,v)) }

    def innerEval(seqInitVal: Valuation): Stream[Valuation] =
      seqEdges.foldLeft(Stream(seqInitVal)) { case (vs, node) => vs.flatMap(v => evaluate(node, v)) }

    def rawStream: Stream[Future[Stream[Valuation]]] = parallelValuations.map(v => Future.apply(innerEval(v).force))

    def next(raw: Stream[Future[Stream[Valuation]]]): Stream[Valuation] = {
      if (raw.isEmpty) Stream.empty
      else {
        raw.take(desiredParallelism).force
        Await.result(raw.head,Duration.Inf) #::: next(raw.tail)
      }
    }

    next(rawStream).toIterator
  }
}
