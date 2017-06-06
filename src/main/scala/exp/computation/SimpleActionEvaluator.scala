package exp.computation

import cats.Eval
import com.typesafe.scalalogging.StrictLogging
import shapeless.{HList, Nat}
import shapeless.ops.hlist.Mapped
import shapeless.syntax.std.traversable.traversableOps
import shapeless.ops.traversable._
import shapeless.ops.nat._

/** Supports the evaluation of `UnsafeCAction`, and works only single-threaded. */
object SimpleActionEvaluator extends StrictLogging {

  case class ActiveVal(v: Valuation, activeNodes: Set[CNode]) {
    def add(n: CNode, value: Any, active: Boolean) = ActiveVal(v + (n,value), activeNodes ++ Some(n).filter(_ => active))
  }
  object ActiveVal {
    def empty: ActiveVal = ActiveVal(Valuation.empty, Set.empty)
  }

  def evalReportsUnsafe(computation: CGraph, channels: Set[UnsafeAction]): Eval[Unit] = {
    val hl = ToSizedHList
    evalReports(computation, channels.toHList)(HList.fill(Nat(channels.size))(Unit)).map(_ => Unit)
  }

  def evalReports[L <: HList, AL <: HList](computation: CGraph, channels: AL)(init: L)(implicit mapped: Mapped.Aux[L,Action,AL]): Eval[L] = {
    implicit val sortingOrder: Ordering[CNode] = Ordering.by(_.isInstanceOf[CedgeDet])
    val topoOrder: List[CNode] = exp.util.topoSort(computation.nodeClosure)

    logger.debug("topological order used for computation: " + topoOrder.mkString(","))

    val zipped: Seq[Either[CNode, UnsafeAction]] = topoOrder.foldLeft[List[CNode Either UnsafeAction]](Nil){
      case (closed,nextNode) =>
        val closedNodes: Set[CNode] = closed.collect{case Left(cn) => cn}.toSet + nextNode
        val closedTables: Set[UnsafeAction] = closed.collect{case Right(t) => t}.toSet
        val sat: Set[UnsafeAction] = channels.filter(t => t.dependencies.forall(closedNodes)) filterNot closedTables
        sat.map(Right(_)).toList ++ (Left(nextNode) :: closed)
    }.reverse

    /** One step expansion. */
    def evaluate(n: CNode, av: ActiveVal): Stream[ActiveVal] = n match {
      case CedgeND(ins,_,f,_,_,_) =>
        f(ins.map(av.v)).zip(true +: Stream.continually(false)).map((av.add(n,_: Any,_: Boolean)).tupled)
      case CedgeDet(ins,_,f,_)    =>
        Stream(av.add(n, f(ins map av.v), ins.forall(av.activeNodes)))
    }

    def channelClosure[T](c: Action[T]): Set[CNode] = exp.graphClosure(c.dependencies)(_.ins)
    import cats.instances.all._
    import cats.syntax.all._
    def process(v: ActiveVal, actions: List[CNode Either UnsafeAction]): Eval[Unit] = actions match {
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
    ???
  }
}
