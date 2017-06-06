package exp.computation

import cats.Eval
import com.typesafe.scalalogging.StrictLogging

/** Supports the evaluation of `UnsafeCAction`, and works only single-threaded. */
object SimpleActionEvaluator extends StrictLogging {

  /** Extends a `Valuation` by keeping track of which nodes are assigned their first value. */
  case class ActiveVal(v: Valuation, activeNodes: Set[CNode]) {
    def add(n: CNode, value: Any, active: Boolean) = ActiveVal(v + (n,value), activeNodes ++ Some(n).filter(_ => active))
    /** Check that all nodes that are not dependencies are in their first assignment. */
    def isActive(dependencies: Set[CNode], cg: CGraph): Boolean =
      (v.assignment.keySet -- exp.graphClosure(dependencies)(_.ins)).forall(activeNodes)
  }

  object ActiveVal {
    def empty: ActiveVal = ActiveVal(Valuation.empty, Set.empty)
  }

  def runGraphUnsafe(computation: CGraph, channels: Set[UnsafeAction]): Eval[Unit] = {
    runGraph(computation, channels.map(identity)).map(_ => Unit)
  }

  def runGraph(computation: CGraph, channels: Set[ActionAny]): Eval[Map[ActionAny,Any]] = {
    implicit val sortingOrder: Ordering[CNode] = Ordering.by(_.isInstanceOf[CedgeDet])
    val topoOrder: List[CNode] = exp.util.topoSort(computation.nodeClosure)

    logger.debug("topological order used for computation: " + topoOrder.mkString(","))

    val zipped: Seq[Either[CNode, ActionAny]] = topoOrder.foldLeft[List[CNode Either ActionAny]](Nil){
      case (closed,nextNode) =>
        val closedNodes: Set[CNode] = closed.collect{case Left(cn) => cn}.toSet + nextNode
        val closedTables: Set[ActionAny] = closed.collect{case Right(t) => t}.toSet
        val sat: Set[ActionAny] = channels.filter(t => t.dependencies.forall(closedNodes)).diff(closedTables)
        sat.map(Right(_)).toList ++ (Left(nextNode) :: closed)
    }.reverse

    /** One step expansion. */
    def evaluate(n: CNode, av: ActiveVal): Stream[ActiveVal] = n match {
      case CedgeND(ins,_,f,_,_,_) =>
        f(ins.map(av.v)).zip(true +: Stream.continually(false)).map((av.add(n,_: Any,_: Boolean)).tupled)
      case CedgeDet(ins,_,f,_)    =>
        Stream(av.add(n, f(ins map av.v), ins.forall(av.activeNodes)))
    }

    def process(v: ActiveVal, actions: List[CNode Either ActionAny], state: Map[ActionAny,Any]): Eval[Map[ActionAny,Any]] = actions match {
      case Nil =>
        Eval.now(state)
      case Left(cn) :: ta =>
        evaluate(cn, v).foldLeft(Eval.now(state))((es,v) => es.flatMap(process(v,ta,_)))
      case Right(oc) :: ta =>
        //perform channel action only if all non-predecessors are either unevaluated, or active (have their first assignment)
        val channelAction = if(v.isActive(oc.dependencies,computation))
          oc.evalUT(v.v,state(oc))
        else
          Eval.now(state(oc))
        channelAction.map(state.updated(oc,_)).flatMap(s => process(v,ta,s))
    }

    process(ActiveVal.empty, zipped.toList, channels.map(c => c -> c.initialUT).toMap)
  }
}
