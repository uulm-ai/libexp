package exp.node

sealed trait Node[+S <: Stage, +T]{
  def dependencies: Seq[Node[Stage,_]]
  def stageDependencies: Seq[Node[S,_]]
  def allNodes: Seq[Node[Stage, _]] = Node.extractNodes(this).distinct
  def allNodesOnStage: Seq[Node[S, _]] = Node.extractStageNodes(this).distinct
}

trait Effort{
  def expectedTime: Double
}
object Effort{
  case class TimeEst(expectedTime: Double) extends Effort
  def none: TimeEst = TimeEst(0d)
  def low: TimeEst = TimeEst(0.01d)
  def moderate: TimeEst = TimeEst(1d)
  def high: TimeEst = TimeEst(10d)
}

case class Length(meanLength: Double)

case class Wrap[S <: Stage, +T](n: Node[S#Next,T]) extends Node[S,T]{
  override def dependencies: Seq[Node[Stage, _]] = Seq(n)
  override def stageDependencies: Seq[Node[S, _]] = Seq()
}
case class App[+S <: Stage,+T](inputs: IndexedSeq[Node[S,_]], f: IndexedSeq[_] => T, effort: Effort = Effort.low, name: Option[String] = None) extends Node[S,T]{
  override def dependencies: Seq[Node[Stage, _]] = inputs
  override def stageDependencies: Seq[Node[S, _]] = inputs
}
object App {
  def point[T](t: T): Node[Base.type, T] = App(IndexedSeq(), _ => t, Effort.none)
  def map[S <: Stage, T, R](n: Node[S,T], effort: Effort = Effort.low, name: Option[String] = None)(f: T => R): Node[S,R] =
    App(IndexedSeq(n), ins => f(ins(0).asInstanceOf[T]), effort, name = name)
  def map2[S1 <: Stage, S2 <: Stage, T1, T2, R](n1: Node[S1,T1], n2: Node[S2,T2], effort: Effort = Effort.low, name: Option[String])(f: (T1,T2) => R)(implicit lub: StageLUB[S1,S2]): Node[lub.Out,R] =
    App(IndexedSeq(lub.lift1(n1),lub.lift2(n2)), (ins: IndexedSeq[_]) => f(ins(0).asInstanceOf[T1], ins(1).asInstanceOf[T2]), name = name, effort = effort)
  def ignoreRight[S1 <: Stage, S2 <: Stage, T1, T2](n1: Node[S1,T1], ignored: Node[S2,T2], name: Option[String] = None)(implicit lub: StageLUB[S1,S2]): Node[lub.Out,T1] =
    App(IndexedSeq(lub.lift1(n1),lub.lift2(ignored)), (ins: IndexedSeq[_]) => ins(0).asInstanceOf[T1], name = name)
}
case class Lift[+S <: Stage,+T](p: Node[S,Stream[T]], perItemEffort: Effort = Effort.none, expectedLength: Length, name: Option[String] = None) extends Node[S,T] {
  override def stageDependencies: Seq[Node[S, _]] = Seq(p)
  override def dependencies: Seq[Node[Stage, _]] = Seq(p)
}
case class Report[+S <: Stage,+T, TT <: T](n: Node[S,T], colName: String, f: TT => String) extends Node[S,T] {
  override def dependencies: Seq[Node[Stage, _]] = Seq(n)
  override def stageDependencies: Seq[Node[S, _]] = Seq(n)
}
case class Inject[+S <: Stage,+T] protected[node] (p: S#Payload[T], name: Option[String] = None) extends Node[S,T] {
  override def dependencies: Seq[Node[Stage, _]] = Seq()
  override def stageDependencies: Seq[Node[S, _]] = Seq()
}

object Node{
  trait Staged[S <: Stage]{
    type N[+T] = Node[S,T]
  }

  def extractNodes(node: Node[Stage,_]): Seq[Node[Stage,_]] =
    node.dependencies.flatMap(dn => extractNodes(dn) :+ dn) :+ node
  def extractStageNodes[S <: Stage](node: Node[S,_]): Seq[Node[S,_]] =
    node.stageDependencies.flatMap(dn => extractStageNodes(dn) :+ dn) :+ node
}
