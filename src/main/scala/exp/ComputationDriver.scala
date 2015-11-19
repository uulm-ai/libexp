package exp

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.syntax.validation._

trait ComputationDriver[T[_]] {
  def evalGraph(query: ClosedQuery, seed: Long): Val[T[Valuation]]
}

/** Single-threaded driver that produces a stream of valuations. Does not minimize resource usage. */
object SimpleStreamDriver extends ComputationDriver[Stream] with StrictLogging {
  def evalGraph(query: ClosedQuery, seed: Long): Val[Stream[Valuation]] = {
    val allNodes = query.allNodes

    @tailrec def topoSort(to: List[Closed[_]]): List[Closed[_]] = {
      val cand = allNodes.filter(n => n.closedDependencies.forall(to.contains) && !to.contains(n))
      if(cand.isEmpty) to.reverse
      else topoSort(cand.head :: to)
    }

    //first node in order is the RNG node, whether it's used or not
    val topoOrder = topoSort(List(RNGSeed))

    logger.info(s"topological order for evaluation: ${topoOrder.mkString(", ")}")

    topoOrder.tail.foldLeft(
      Stream(Valuation(Map(RNGSeed.name -> seed)))
    ){
      case (v,edge: ClosedEdge[_]) => v.flatMap(edge.valuationStream)
      case (_, RNGSeed) => sys.error("this should not happen")
    }.successNel
  }
}