package exp

import com.typesafe.scalalogging.StrictLogging
import fastparse.all._
import fastparse.core.Result._
import shapeless._
import scala.annotation.tailrec
import scalaz.Validation

case class CliOpt[+T](long: String, short: Option[Char], valueParser: P[T], default: Option[T], description: String){
  def argNameP: P[Unit] = P(("--" ~ long) | ("-" ~ short.map(c => CharIn(Seq(c))).getOrElse(P(Fail))))
  def cliParser(sep: P[Unit]): P[T] = argNameP ~ sep ~ valueParser
}

object CliOpt {
  val sepChar = '\t'
  val sepP: P[Unit] = P(CharIn(Seq(sepChar)))

  def parse[LUB](args: Array[String], opts: Seq[CliOpt[LUB]]): Validation[String,Seq[LUB]] = {
    val p: P[Seq[LUB]] = opts.map(opt => opt.cliParser(sepP)).reduce(_ | _).rep(sep = sepP)

    val either = p.parse(args.mkString(sepChar.toString)) match {
      case Failure(x,y) => Left(s"failed to parse input: $x")
      case Success(value,_)      => Right(value)
    }
    Validation.fromEither(either)
  }
}

object Driver extends StrictLogging {
  def nodeClosure[A](query: Iterable[A])(pred: A => Iterable[A]): Set[A] =
    Iterator.iterate(query.toSet)(found => found.flatMap(pred) ++ found)
      .sliding(2)
      .dropWhile(two => two(0).size != two(1).size)
      .next().head

  def parseCli(query: Set[Node[_]], args: Array[String]): Validation[String,Seq[Closed[_]]] = {
    import Validation._
    import scalaz.{Node => _, _}
    import Scalaz._

    val nodes: Set[Node[_]] = nodeClosure(query){
      case e: Edge[_] => e.dependencies
      case _ => Set()
    }

    val cliInputs: List[FromString[Any]] = nodes.collect{
      case fs: FromString[_] => fs
    }.toList

    val result: Validation[String, Seq[Closed[Any]]] = CliOpt.parse(args, cliInputs.toSeq.map(_.cliOpt))

    for{
      parsed <- result
      parsedMap: Map[String, Seq[Closed[Any]]] = parsed.groupBy(_.name)
      allClosed <- cliInputs.map(ci =>
        parsedMap
          .get(ci.name).flatMap(_.headOption)
          .orElse(ci.default)
          .fold[Validation[String,Closed[Any]]](failure(s"argument '--${ci.cliOpt.long}' is required"))(success)
      ).sequenceU
    } yield allClosed
  }

  def closeGraph(query: Iterable[Node[_]], substitution: Map[String,Closed[_]]): Iterable[Closed[_]] = {
    def close(n: Node[_]): Closed[_] = substitution.get(n.name) match {
      case Some(c) => c
      case None =>
        n match {
          case cl: Closed[_] => cl
          case fs: FromString[_] => substitution.getOrElse(fs.name, sys.error("this should not happen: input node not fpund in CLI input"))
          case e: Edge[_] => e.close(close)
        }
    }
    query.map(close)
  }

  def evalGraph(query: Iterable[Closed[_]], seed: Long): Stream[Valuation] = {
    val allNodes: Set[Closed[_]] = nodeClosure(query)(_.closedDependencies)

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

  def run(query: Set[Node[_]], args: Array[String]): Unit = {
    import scalaz.std.string._

    val r = for{
      inputs <- parseCli(query, args)
      _ = logger.info("parsed cli input")
      closed = closeGraph(query, inputs.groupBy(_.name).mapValues(_.head))
      _ = logger.info("closed graph")
      result: Stream[Valuation] = evalGraph(closed, 0L)
    } yield result
    r.foreach(rs => println(rs.mkString("\n")))
  }
}

