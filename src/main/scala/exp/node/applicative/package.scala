package exp.node

import com.typesafe.scalalogging.StrictLogging
import exp.cli._
import exp.computation.{Column => CCol,_}
import scalaz.{Node => _,_}
import scalaz.std.list._
import scalaz.syntax.traverse._


package object applicative extends StrictLogging {

  object syntax extends NodeSyntax {

    type N[+T] = exp.node.applicative.Node[T]

    override def lift[T, S](n: N[T], estimatedLength: Double, name: String)(implicit ev: <:<[T, Iterable[S]]): N[S] =
      Lift(n, estimatedLength, name)(ev)

    override def mAppNUntyped[R](nodes: IndexedSeq[N[Any]], f: (IndexedSeq[Any]) => R, name: String, effort: Effort): N[R] =
      MApp(nodes, f, name, effort)

    override def pure[T](x: T, name: String): N[T] = Pure(x, name)

    override def cli[T](name: String, parser: Read[T], description: String, format: String = "", default: Option[T] = None): N[T] =
      Cli(CliOpt(name, parser, description, short = None, default = default, formatDescription = format))

    override def addColumn[T](n: N[T], name: String, f: T => String): N[T] = Column(n, name, (a: Any) => f(a.asInstanceOf[T]))

    override def seed(name: String): N[Long] = Seed(name)

    override def ignore[T](taken: N[T], ignored: N[Any]): N[T] = ^(taken,ignored, s"ignore.${ignored.name}")((t,i) => t)
  }

  import syntax._

  def buildCGraph(n: N[Any], cliValuation: Cli[Any] => Any, seeds: Seq[Long]): CGraph = {

    def printAllNodes(x: N[Any], msg: String): Unit = logger.info(
      s"$msg:\n\t- " + exp.graphClosure(Seq(x))(_.predecessors).toSeq.map(n => s"${n.productPrefix}: ${n.name}").sorted.mkString("\n\t- ")
    )

    printAllNodes(n,"input")

    val cliReplaced = new ~>[N, N] {
      override def apply[A](fa: N[A]): N[A] = fa match {
        case cli: Cli[A] =>
          pure(cliValuation(cli).asInstanceOf[A], cli.name)
        case otherwise =>
          otherwise.mapNodes(this)
      }
    }.apply(n)

    printAllNodes(cliReplaced, "cliReplaced")

    val seedInserted = new ~>[N, N] {
      override def apply[A](fa: N[A]): N[A] = fa match {
        case Seed(name) => pure(seeds, name).lift(seeds.length, s"$name.lifted").asInstanceOf[N[A]]
        case otherwise => otherwise.mapNodes(this)
      }
    }.apply(cliReplaced)

    val (withoutColumnsMap,columns) = exp.topologicalOrder(Seq(seedInserted))(_.predecessors).foldLeft((Map[N[Any],N[Any]](),Set[Column[Any]]())){
      case ((m,cs),c@Column(pred, name, rep)) =>
        (m + (c -> m(pred)), cs + Column(m(pred),name,rep))
      case ((m,cs),other) =>
        (m + (other -> other.mapNodes(new ~>[N,N]{
          override def apply[A](fa: N[A]): N[A] = m(fa).asInstanceOf[N[A]]
        })),cs)
    }

    val withoutColumns = withoutColumnsMap(seedInserted)

    printAllNodes(withoutColumns, "withoutColumns")

    //`withoutColumns` only contains `Pure`, `Lift`, `MApp` nodes
    val mapToCEdge: Map[N[Any], CEdge] = exp.topologicalOrder(Seq(withoutColumns))(_.predecessors).foldLeft(Map[N[Any],CEdge]()){
      case (m, p@Pure(value,name)) => m + (p -> CEdge.fromSeq(Seq(value),name))
      case (m, l@Lift(pred, length, name)) => m + (l -> CedgeND(IndexedSeq(m(pred)), name, ins => l.toIterable(ins(0)).toStream, length, 0d, 0d))
      case (m, mapp@MApp(preds, f, name, effort)) => m + (mapp -> CedgeDet(preds.map(m), name, f, effort.expectedTime))
      case (_,otherwise) => sys.error("mapToCEdge: encountered unexpected node type: " + otherwise)
    }

    CGraph(Set(mapToCEdge(withoutColumns)), columns.toSeq.map(col => exp.computation.Column(col.name,mapToCEdge(col.pred), col.reporter)))
  }

  def createParser(n: N[Any]): CLI[Seq[Long] => CGraph] = {
    //collect cli nodes
    val allCliNodes: List[Cli[Any]] = exp.graphClosure(Seq(n))(_.predecessors).collect{
      case cli: Cli[Any] => cli
    }.toList.sortBy(_.name)

    allCliNodes
      .map(cli => liftCliOpt(cli.parser))
      .sequence
      .map(parsedArgs =>
        (seeds: Seq[Long]) => buildCGraph(n, allCliNodes.zip(parsedArgs).toMap, seeds)
      )
  }
}

