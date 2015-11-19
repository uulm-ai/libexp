package exp

import scala.annotation.tailrec
import com.typesafe.scalalogging.StrictLogging

import scalaz.std.list._
import scalaz.syntax.validation._
import scalaz.syntax.traverse._

class OpenQuery protected[OpenQuery](val queryNodes: Seq[Node[_]]){
  lazy val allNodes: Seq[Node[_]] = Driver.nodeClosure(queryNodes){
    case e: Edge[_] => e.dependencies
    case _ => Set()
  }.toSeq

  lazy val openNodes: Seq[FromCLI[Any]] = allNodes.collect{
    case fs: FromCLI[_] => fs
  }

  def close(substitution: Map[String,Closed[_]]): Val[ClosedQuery] = {
    def closeI(n: Node[_]): Val[Closed[_]] = substitution.get(n.name) match {
      case Some(c) => c.successNel
      case None =>
        n match {
          case cl: Closed[_] => cl.successNel
          case fs: FromCLI[_] =>
            substitution
              .get(fs.name)
              .orElse(fs.default)
              .fold[Val[Closed[_]]](s"missing required argument '--${fs.cliOpt.long}'".failureNel)(a => a.successNel)
          case e: Edge[_] => e.close[Val](closeI)
        }
    }

    allNodes.toList.map(closeI).sequence.map(ClosedQuery(_))
  }

  override def hashCode(): Int = queryNodes.toSet.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case q: OpenQuery => q.queryNodes.toSet == queryNodes.toSet
    case _ => false
  }
}

object OpenQuery {
  def apply(qNs: Iterable[Node[_]]): OpenQuery = new OpenQuery(qNs.toSeq.distinct)
  def apply(qNs: Node[_]*): OpenQuery = new OpenQuery(qNs.toSeq.distinct)
}

class ClosedQuery protected[ClosedQuery](val queryNodes: Seq[Closed[_]]) {
  lazy val allNodes: Seq[Closed[_]] = Driver.nodeClosure(queryNodes)(_.closedDependencies).toSeq

  override def hashCode(): Int = queryNodes.toSet.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case cq: ClosedQuery => cq.queryNodes.toSet == queryNodes.toSet
    case _ => false
  }
}

object ClosedQuery {
  def apply(qNs: Iterable[Closed[_]]) = new ClosedQuery(qNs.toSeq.distinct)
}

object Driver extends StrictLogging {
  def nodeClosure[A](query: Iterable[A])(pred: A => Iterable[A]): Set[A] =
    Iterator.iterate(query.toSet)(found => found.flatMap(pred) ++ found)
      .sliding(2)
      .dropWhile(two => two(0).size != two(1).size)
      .next().head

  def parseCli(query: OpenQuery, args: Array[String]): Val[ClosedQuery] = {
    val openNodes = query.openNodes.toSeq
    for {
      cliMapping <- CliOpt.parse[Closed[Any]](args, openNodes.map(_.cliOpt))
      parsedMap = openNodes.map(_.name).zip(cliMapping).toMap
      _ = logger.info(s"parsed from arguments: [${parsedMap.mkString("; ")}]")
      closed <- query.close(parsedMap)
    } yield closed
  }

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

