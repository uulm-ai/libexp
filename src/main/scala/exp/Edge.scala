package exp

import com.typesafe.scalalogging.StrictLogging

import scalaz.std.list._
import scalaz.syntax.validation._
import scalaz.syntax.traverse._

class OpenQuery protected[OpenQuery](val queryNodes: Seq[Node[_]]) extends StrictLogging {
  lazy val allNodes: Seq[Node[_]] = graphClosure(queryNodes){
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

  /** Use the provided command-line arguments to close this query. */
  def parseCli(args: Array[String]): Val[ClosedQuery] = for {
    cliMapping <- CliOpt.parse[Closed[Any]](args, openNodes.map(_.cliOpt))
    parsedMap = openNodes.map(_.name).zip(cliMapping).toMap
    _ = logger.info(s"parsed from arguments: [${parsedMap.mkString("; ")}]")
    closed <- close(parsedMap)
  } yield closed
}

object OpenQuery {
  def apply(qNs: Iterable[Node[_]]): OpenQuery = new OpenQuery(qNs.toSeq.distinct)
  def apply(qNs: Node[_]*): OpenQuery = new OpenQuery(qNs.toSeq.distinct)
}

class ClosedQuery protected[ClosedQuery](val queryNodes: Seq[Closed[_]]) {
  lazy val allNodes: Seq[Closed[_]] = graphClosure(queryNodes)(_.closedDependencies).toSeq

  override def hashCode(): Int = queryNodes.toSet.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case cq: ClosedQuery => cq.queryNodes.toSet == queryNodes.toSet
    case _ => false
  }
}

object ClosedQuery {
  def apply(qNs: Iterable[Closed[_]]) = new ClosedQuery(qNs.toSeq.distinct)
}

