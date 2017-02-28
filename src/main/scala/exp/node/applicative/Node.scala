package exp.node.applicative

import cats.~>
import exp.cli.CliOpt
import exp.node.{DVariable, DescribableExperiment, Effort, IVariable}

sealed trait Node[+T] extends Product {
  def name: String
  def predecessors: IndexedSeq[Node[Any]]
  def mapNodes(f: Node ~> Node): Node[T]
}

object Node {
  implicit val describeInstance: DescribableExperiment[Node[Any]] = new DescribableExperiment[Node[Any]] {
    override def experimentHash(a: Node[Any]): Long = ???
    override def inputs(a: Node[Any]): Seq[IVariable] = exp.graphClosure(Set(a))(_.predecessors).toSeq.sortBy(_.name).collect{
      case Cli(CliOpt(name, _, desc, _, _, _)) => IVariable(name, None, Some(desc).filter(_.nonEmpty))
      case Pure(_, name) => IVariable(name, None, None)
    }
    override def outputs(a: Node[Any]): Seq[DVariable] = exp.graphClosure(Set(a))(_.predecessors).toSeq.sortBy(_.name).collect{
      case Column(_, name, _) => DVariable(name,None, None)
    }
  }
}

sealed trait SourceNode[+T] extends Node[T] {
  override def predecessors: IndexedSeq[Node[Any]] = IndexedSeq.empty
  override def mapNodes(f: ~>[Node, Node]): Node[T] = this
}
case class Pure[+T](value: T, name: String) extends SourceNode[T]{
  override def predecessors: IndexedSeq[Node[Any]] = IndexedSeq.empty
}
case class Seed(name: String) extends SourceNode[Long]{
  override def predecessors: IndexedSeq[Node[Any]] = IndexedSeq.empty
}
case class Cli[+T](parser: CliOpt[T]) extends SourceNode[T]{
  override def name: String = parser.long
  override def predecessors: IndexedSeq[Node[Any]] = IndexedSeq.empty
}

case class Lift[+T,Ts](pred: Node[Ts], estimatedLength: Double, name: String)(implicit val toIterable: Ts <:< Iterable[T]) extends Node[T] {
  override def predecessors: IndexedSeq[Node[Any]] = IndexedSeq(pred)
  override def mapNodes(f: ~>[Node, Node]): Node[T] = this.copy(pred = f(pred))
}

case class Column[T](pred: Node[T], name: String, reporter: Any => String) extends Node[T]{
  override def predecessors: IndexedSeq[Node[Any]] = IndexedSeq(pred)
  override def mapNodes(f: ~>[Node, Node]): Node[T] = this.copy(pred = f(pred))
}

case class MApp[+T](predecessors: IndexedSeq[Node[Any]], f: IndexedSeq[Any] => T, name: String, effort: Effort) extends Node[T] {
  override def mapNodes(f: ~>[Node, Node]): Node[T] = this.copy(predecessors = predecessors.map(f.apply))
}

