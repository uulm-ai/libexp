package exp.computation

import cats.Eval
import com.typesafe.scalalogging.StrictLogging
import exp.node

object SimpleTabledEvaluator extends StrictLogging {

  case class ActiveVal(v: Valuation, activeNodes: Set[CNode]) {
    def add(n: CNode, value: Any, active: Boolean) = ActiveVal(v + (n,value), activeNodes ++ Some(n).filter(_ => active))
  }
  object ActiveVal {
    def empty: ActiveVal = ActiveVal(Valuation.empty, Set.empty)
  }

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

    /** One step expansion. */
    def evaluate(n: CNode, av: ActiveVal): Stream[ActiveVal] = n match {
      case CedgeND(ins,_,f,_,_,_) =>
        f(ins.map(av.v)).zip(true +: Stream.continually(false)).map((av.add(n,_: Any,_: Boolean)).tupled)
      case CedgeDet(ins,_,f,_)    =>
        Stream(av.add(n, f(ins map av.v), ins.forall(av.activeNodes)))
    }

    def channelClosure[T](c: Channel[T]): Set[CNode] = exp.graphClosure(c.nodes)(_.ins)
    import cats.instances.all._
    import cats.syntax.all._
    def process(v: ActiveVal, actions: List[CNode Either UnsafeChannel]): Eval[Unit] = actions match {
      case Nil =>
        Eval.Unit
      case Left(cn) :: ta =>
        evaluate(cn, v).map(process(_,ta)).sequence.map(_ => Unit)
      case Right(oc) :: ta =>
        //perform channel action only if all non-predecessors are either unevaluated, or active (have their first assignment)
        val channelAction = if((v.v.assignment.keySet -- channelClosure(oc)).forall(v.activeNodes))
          oc.evalUnsafe(v.v)
        else
          Eval.Unit
        channelAction >> process(v,ta)
    }

    process(ActiveVal.empty, zipped.toList)
  }
}
