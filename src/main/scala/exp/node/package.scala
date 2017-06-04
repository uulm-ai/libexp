package exp

import cats.instances.list._
import cats.syntax.traverse._
import cats.~>
import com.typesafe.scalalogging.StrictLogging
import exp.cli._
import exp.computation._

package object node extends StrictLogging {

  trait NodeSyntax extends AbstractNodeSyntax {

    override def name(n: N[Any]): String = n.name

    type N[+T] = Node[T]

    override def lift[T, S](n: N[T], estimatedLength: Double, name: String)(implicit ev: <:<[T, Iterable[S]]): N[S] =
      Lift(n, estimatedLength, name)(ev)

    override def mAppNUntyped[R](nodes: IndexedSeq[N[Any]], f: (IndexedSeq[Any]) => R, name: String, effort: Effort): N[R] =
      MApp(nodes, f, name, effort)

    override def pure[T](x: T, name: String): N[T] = Pure(x, name)

    override def cli[T](name: String, parser: Read[T], description: String, format: String = "", default: Option[T] = None): N[T] =
      Cli(CliOpt(name, parser, description, short = None, default = default, formatDescription = format))

    override def addColumn[T](n: N[T], name: String, f: T => String, tables: Set[Table] = Set(Table.default)): N[T] =
      Column(n, name, (a: Any) => f(a.asInstanceOf[T]), tables)

    override def ignore[T](taken: N[T], ignored: N[Any]): N[T] = ^(taken,ignored, s"ignore.${ignored.name}")((t,i) => t)
  }

  object syntax extends NodeSyntax

  import syntax._

  def logNode(n: N[Any], msg: String): Unit = {
    logger.debug(s"$msg:\n\t" + exp.graphClosure(Seq(n))(_.predecessors).toSeq.sortBy(_.name).map(n => s"${n.productPrefix}:${n.name}").mkString("\n\t"))
  }

  def buildCGraph(n: N[Any], cliValuation: Cli[Any] => Any): CGraph = {

    //replace Cli, Column at once and filter out Column
    val (withoutColumnsMap, columns: Set[Column[Any]]) = exp
      .topologicalOrder(Seq(n))(_.predecessors)
      .foldLeft((Map[N[Any],N[Any]](),Set[Column[Any]]())){
        case ((subst,cols),next) =>
          next match {
            case cli: Cli[Any] =>
              (subst + (cli -> pure(cliValuation(cli), cli.name)), cols)
            case c@Column(pred, name, rep, tables) =>
              val substitutedPredecessor = subst(pred)
              (subst + (c -> substitutedPredecessor), cols + Column(substitutedPredecessor,name,rep,tables))
            case otherwise =>
              (subst + (otherwise -> otherwise.mapNodes(new ~>[N,N]{
                override def apply[A](fa: N[A]): N[A] = subst(fa).asInstanceOf[N[A]]
              })), cols)
          }
      }

    val withoutColumns: N[Any] = withoutColumnsMap(n)

    logNode(withoutColumns, "nodes in computation graph after substitution of Column, Seed, Cli")

    //`withoutColumns` only contains `Pure`, `Lift`, `MApp` nodes
    val mapToCEdge: Map[N[Any], CNode] = exp
      .topologicalOrder(Seq(withoutColumns))(_.predecessors)
      .foldLeft(Map[N[Any],CNode]()){
        case (m, p@Pure(value,name)) => m + (p -> CNode.pure(value,name))
        case (m, l@Lift(pred, length, name)) => m + (l -> CedgeND(IndexedSeq(m(pred)), name, ins => l.toIterable(ins(0)).toStream, length, 0d, 0d))
        case (m, mapp@MApp(preds, f, name, effort)) => m + (mapp -> CedgeDet(preds.map(m), name, f, effort.expectedTime))
        case (_, otherwise) => sys.error("mapToCEdge: encountered unexpected node type: " + otherwise)
      }

    val mappedColumns: Seq[computation.CColumn] =
      columns.toSeq.map(col => exp.computation.CColumn(col.name, mapToCEdge(col.pred), col.reporter))

    CGraph(Set(mapToCEdge(withoutColumns)), mappedColumns)
  }

  def nodeClosure(n: N[Any]): Set[Node[Any]] = exp.graphClosure(Seq(n))(_.predecessors)

  def createParser(n: N[Any]): CLI[CGraph] = {
    //collect cli nodes
    val allCliNodes: List[Cli[Any]] = nodeClosure(n).collect{
      case cli: Cli[Any] => cli
    }.toList.sortBy(_.name)

    //check for duplicated names and warn
    allCliNodes.groupBy(_.name).filter(_._2.distinct.size > 1)
      .foreach(dup => logger.warn("different nodes with same name found: " + dup))

    allCliNodes
      .map(cli => liftCliOpt(cli.parser))
      .sequence
      .map((parsedArgs: List[Any]) => buildCGraph(n, allCliNodes.zip(parsedArgs).toMap))
  }
}

