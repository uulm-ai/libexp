package exp

import scala.annotation.tailrec
import com.typesafe.scalalogging.StrictLogging

import scalaz._
import Scalaz._
import Validation._

class OpenQuery protected[OpenQuery](val queryNodes: Seq[Node[_]]){
  lazy val allNodes: Seq[Node[_]] = Driver.nodeClosure(queryNodes){
    case e: Edge[_] => e.dependencies
    case _ => Set()
  }.toSeq

  lazy val openNodes: Seq[FromString[Any]] = allNodes.collect{
    case fs: FromString[_] => fs
  }

  def close(substitution: Map[String,Closed[_]]): Val[ClosedQuery] = {
    def closeI(n: Node[_]): Val[Closed[_]] = substitution.get(n.name) match {
      case Some(c) => c.successNel
      case None =>
        n match {
          case cl: Closed[_] => cl.successNel
          case fs: FromString[_] =>
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

  def parseCli(query: OpenQuery, args: Array[String]): Val[ClosedQuery] = for {
    cliMapping <- CliOpt.parse(args, query.openNodes.toSeq.map(_.cliOpt))
    parsedMap: Map[String, Seq[Closed[Any]]] = cliMapping.groupBy(_.name)
    injection <- parsedMap.map{
      case (key, Seq(v))=> (key -> v).successNel
      case (key, vals)  => s"parameter $key may only be given once".failureNel
    }.toList.sequenceU
    closed <- query.close(injection.toMap)
  } yield closed

  def evalGraph(query: ClosedQuery, seed: Long): Stream[Valuation] = {
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
    }
  }

  def run(query: Iterable[Node[_]], args: Array[String]): Unit = {
    val r = for{
      closed <- parseCli(OpenQuery(query), args)
      _ = logger.info("parsed cli input")
      result: Stream[Valuation] = evalGraph(closed, 0L)
    } yield result
    r.foreach(rs => println(rs.mkString("\n")))
  }
}

