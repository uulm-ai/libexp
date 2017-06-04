package exp.computation

import cats.Eval
import com.typesafe.scalalogging.StrictLogging

object SimpleTabledEvaluator extends StrictLogging {

  def evalReportsUnsafe(computation: CGraph, channels: Set[UnsafeChannel]): Eval[Unit] = {
    implicit val sortingOrder: Ordering[CNode] = Ordering.by(_.isInstanceOf[CedgeDet])
    val topoOrder: List[CNode] = exp.util.topoSort(computation.nodeClosure)

    logger.debug("topological order used for computation: " + topoOrder.mkString(","))

    val zipped: Seq[Either[CNode, UnsafeChannel]] = topoOrder.foldLeft[List[CNode Either UnsafeChannel]](Nil){
      case (closed,nextNode) =>
        val closedNodes: Set[CNode] = closed.collect{case Left(cn) => cn}.toSet + nextNode
        val closedTables: Set[UnsafeChannel] = closed.collect{case Right(t) => t}.toSet
        val sat: Set[UnsafeChannel] = channels.filter(t => t.nodes.forall(closedNodes)) filterNot closedTables
        sat.map(Right(_)).toList ++ (Left(nextNode) :: closed)
    }.reverse

    def evaluate(n: CNode, valuation: Valuation): Stream[Valuation] = n match {
      case CedgeND(ins,_,f,_,_,_) =>
        f(ins.map(valuation.apply)).map(x => valuation + (n -> x))
      case CedgeDet(ins,_,f,_)    =>
        Stream(valuation + (n -> f(ins map valuation.apply)))
    }

    import cats.instances.all._
    import cats.syntax.all._
    def process(v: Valuation, actions: List[CNode Either UnsafeChannel]): Eval[Unit] = actions match {
      case Nil =>
        Eval.Unit
      case Left(cn) :: ta =>
        evaluate(cn, v).map(process(_,ta)).sequence.map(_ => Unit)
      case Right(oc) :: ta =>
        oc.evalUnsafe(v) >> process(v,ta)
    }

    process(Valuation.empty, zipped.toList)
  }
}
