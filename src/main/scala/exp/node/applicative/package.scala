package exp.node

import com.typesafe.scalalogging.StrictLogging
import exp.cli._
import exp.computation
import exp.computation.{Column => _,_}
import scalaz.{Node => _,_}
import scalaz.std.list._
import scalaz.syntax.traverse._


package object applicative extends StrictLogging {

  object syntax extends NodeSyntax {

    override def name(n: N[Any]): String = n.name

    type N[+T] = exp.node.applicative.Node[T]

    override def lift[T, S](n: N[T], estimatedLength: Double, name: String)(implicit ev: <:<[T, Iterable[S]]): N[S] =
      Lift(n, estimatedLength, name)(ev)

    override def mAppNUntyped[R](nodes: IndexedSeq[N[Any]], f: (IndexedSeq[Any]) => R, name: String, effort: Effort): N[R] =
      MApp(nodes, f, name, effort)

    override def pure[T](x: T, name: String): N[T] = Pure(x, name)

    override def cli[T](name: String, parser: Read[T], description: String, format: String = "", default: Option[T] = None): N[T] =
      Cli(CliOpt(name, parser, description, short = None, default = default, formatDescription = format))

    override def addColumn[T](n: N[T], name: String, f: T => String): N[T] = Column(n, name, (a: Any) => f(a.asInstanceOf[T]))

    override def seed(name: String): N[Long] = Seed(name).addColumn(s"seed.$name")

    override def ignore[T](taken: N[T], ignored: N[Any]): N[T] = ^(taken,ignored, s"ignore.${ignored.name}")((t,i) => t)
  }

  import syntax._

  def logNode(n: N[Any], msg: String): Unit = {
    logger.debug(s"$msg:\n\t" + exp.graphClosure(Seq(n))(_.predecessors).toSeq.sortBy(_.name).map(n => s"${n.productPrefix}:${n.name}").mkString("\n\t"))
  }

  def buildCGraph(n: N[Any], cliValuation: Cli[Any] => Any, seeds: Seq[Long]): CGraph = {

    //we do NOT add the column for `base.seed` here, as this will get substituted into the DAG and won't work with
    //the rewriting algorithm below; instead we add a exp.computation.Column directly further down
    val baseSeed = pure(seeds, "base.seed").lift(seeds.length)

    //replace Cli, Seed, Column at once and filter out Column
    val (withoutColumnsMap, columns) = exp.topologicalOrder(Seq(n))(_.predecessors).foldLeft((Map[N[Any],N[Any]](),Set[Column[Any]]())){ case ((subst,cols),next) =>
      next match {
        case cli: Cli[Any] =>
          (subst + (cli -> pure(cliValuation(cli), cli.name)), cols)
        case s@Seed(name) =>
          (subst + (s -> baseSeed.map(_ ^ name.hashCode)), cols)
        case c@Column(pred, name, rep) =>
          val substitutedPredecessor = subst(pred)
          (subst + (c -> substitutedPredecessor), cols + Column(substitutedPredecessor,name,rep))
        case otherwise =>
          (subst + (otherwise -> otherwise.mapNodes(new ~>[N,N]{
            override def apply[A](fa: N[A]): N[A] = subst(fa).asInstanceOf[N[A]]
          })), cols)
      }
    }

    val withoutColumns: N[Any] = withoutColumnsMap(n)

    logNode(withoutColumns, "nodes in computation graph after substitution of Column, Seed, Cli")

    //`withoutColumns` only contains `Pure`, `Lift`, `MApp` nodes
    val mapToCEdge: Map[N[Any], CNode] = exp.topologicalOrder(Seq(withoutColumns))(_.predecessors).foldLeft(Map[N[Any],CNode]()){
      case (m, p@Pure(value,name)) => m + (p -> CNode.fromSeq(Seq(value),name))
      case (m, l@Lift(pred, length, name)) => m + (l -> CedgeND(IndexedSeq(m(pred)), name, ins => l.toIterable(ins(0)).toStream, length, 0d, 0d))
      case (m, mapp@MApp(preds, f, name, effort)) => m + (mapp -> CedgeDet(preds.map(m), name, f, effort.expectedTime))
      case (_,otherwise) => sys.error("mapToCEdge: encountered unexpected node type: " + otherwise)
    }

    if(!mapToCEdge.contains(baseSeed))
      logger.info("not adding random seed node because no seed requested")
    //only add a column for the base.seed when there is a seed node
    val mappedColumns: Seq[computation.Column] =
      columns.toSeq.map(col => exp.computation.Column(col.name, mapToCEdge(col.pred), col.reporter)) ++
        ( if(mapToCEdge.contains(baseSeed)) Seq(exp.computation.Column("base.seed", mapToCEdge(baseSeed), _.toString))
          else Seq() )

    CGraph(Set(mapToCEdge(withoutColumns)), mappedColumns.sortBy(_.name))
  }

  def createParser(n: N[Any]): CLI[Seq[Long] => CGraph] = {
    //collect cli nodes
    val allCliNodes: List[Cli[Any]] = exp.graphClosure(Seq(n))(_.predecessors).collect{
      case cli: Cli[Any] => cli
    }.toList.sortBy(_.name)

    allCliNodes
      .map(cli => liftCliOpt(cli.parser))
      .sequence
      .map((parsedArgs: List[Any]) =>
        (seeds: Seq[Long]) => buildCGraph(n, allCliNodes.zip(parsedArgs).toMap, seeds)
      )
  }
}

