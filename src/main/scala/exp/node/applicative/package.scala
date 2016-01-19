package exp.node

import exp.cli._
import exp.computation._
import scalaz.{Node => _,_}
import scalaz.std.list._
import scalaz.syntax.traverse._


package object applicative{

  object syntax extends NodeSyntax {

    type N[+T] = exp.node.applicative.Node[T]

    override def lift[T, S](n: N[T], estimatedLength: Double, name: String)(implicit ev: <:<[T, Iterable[S]]): N[S] =
      Lift(n, estimatedLength, name)(ev)

    override def mAppNUntyped[R](nodes: IndexedSeq[N[Any]], f: (IndexedSeq[Any]) => R, name: String, effort: Effort): N[R] =
      MApp(nodes, f, name, effort)

    override def pure[T](x: T, name: String): N[T] = Pure(x, name)

    override def cli[T](name: String, parser: Read[T], description: String, format: String = "", default: Option[T] = None): N[T] =
      Cli(CliOpt(name, parser, description, short = None, default = default, formatDescription = format))

    override def addColumn[T](n: N[T], name: String, f: T => String): N[T] = Column(n, name, (a: Any) => f(a.asInstanceOf[T])) *> n

    override def seed(name: String): N[Long] = Seed(name)
  }

  import syntax._

  def buildCGraph(n: N[Any], cliValuation: Cli[Any] => Any, seeds: Seq[Long]): CGraph = {

    val cliReplaced = n.mapNodes(new ~>[N,N]{
      override def apply[A](fa: N[A]): N[A] = fa match {
        case cli: Cli[A] => pure(cliValuation(cli).asInstanceOf[A], cli.name)
        case otherwise => otherwise
      }
    })

    val seedInserted = cliReplaced.mapNodes(new ~>[N,N]{
      override def apply[A](fa: N[A]): N[A] = fa match {
        case Seed(name) => pure(seeds, name).asInstanceOf[N[A]]
        case otherwise  => otherwise
      }
    })

    val columns: Set[Column] = exp.graphClosure(Seq(seedInserted))(_.predecessors).collect{case c: Column => c}

    val columnToUnitPipe: N ~> N = new ~>[N,N] {
      override def apply[A](fa: N[A]): N[A] = fa match {
        case c: Column => MApp(IndexedSeq(fa.n), _ => Unit, "fa.name.unit", Effort.none).asInstanceOf[N[A]]
        case otherwise => otherwise
      }
    }

    val withoutColumns = seedInserted.mapNodes(columnToUnitPipe)

    //`withoutColumns` only contains `Pure`, `Lift`, `MApp` nodes

    val mapToCEdge: Map[N[Any], CEdge] = exp.topologicalOrder(Seq(withoutColumns))(_.predecessors).foldLeft(Map[N[Any],CEdge]()){
      case (m, p@Pure(value,name)) => m + (p -> CEdge.fromSeq(Seq(value),name))
      case (m, l@Lift(pred, length, name)) => m + (l -> CedgeND(IndexedSeq(m(pred)), name, ins => l.toIterable(ins(0)).toStream, length, 0d, 0d))
      case (m, mapp@MApp(preds, f, name, effort)) => m + (mapp -> CedgeDet(preds.map(m), name, f, effort.expectedTime))
      case otherwise => sys.error("encountered unexpected node type: " + otherwise)
    }

    CGraph(Set(mapToCEdge(n)), columns.toSeq.map(col => exp.computation.Column(col.name,mapToCEdge(col.pred), col.reporter)))
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

