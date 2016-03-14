package exp.computation

import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global

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

    logger.info(s"found parallelization scheme:\n\tparallel:\t${parEdges.map(_.name).mkString(",")}\n\tsequential:\t${seqEdges.map(_.name).mkString(",")}")

    def evaluate(n: CEdge, valuation: Valuation): Stream[Valuation] = n match {
      case CedgeND(ins,_,f,_,_,_) =>
        f(ins.map(valuation.apply)).map(x => valuation + (n -> x))
      case CedgeDet(ins,_,f,_)    =>
        Stream(valuation + (n -> f(ins map valuation.apply)))
    }

    val parallelValuations: Stream[Valuation] = parEdges.foldLeft(Stream(Valuation(Map()))){case (vs,node) =>
      vs.flatMap(v => evaluate(node,v))
    }

    def innerEval(seqInitVal: Valuation): Stream[Valuation] =
      seqEdges.foldLeft(Stream(seqInitVal)) { case (vs, node) =>
        vs.flatMap(v => evaluate(node, v))
      }

    def getInt(name: String, default: String) = (try System.getProperty(name, default) catch {
      case e: SecurityException => default
    }) match {
      case s if s.charAt(0) == 'x' => (Runtime.getRuntime.availableProcessors * s.substring(1).toDouble).ceil.toInt
      case other => other.toInt
    }

    def range(floor: Int, desired: Int, ceiling: Int) = scala.math.min(scala.math.max(floor, desired), ceiling)

    val desiredParallelism = range(
      getInt("scala.concurrent.context.minThreads", "1"),
      getInt("scala.concurrent.context.numThreads", "x1"),
      getInt("scala.concurrent.context.maxThreads", "x1"))

    val rawStream: Stream[Future[Stream[Valuation]]] = parallelValuations.map(v => Future.apply(innerEval(v).force))

    def next(raw: Stream[Future[Stream[Valuation]]]): Stream[Valuation] = {
      if (raw.isEmpty) Stream.empty
      else {
        raw.drop(desiredParallelism)
        Await.result(raw.head,Duration.Inf) #::: next(raw.tail)
      }
    }

    next(rawStream)
  }
}
