package exp.node


sealed trait Node[+S <: Stage, +T]{
  def stage: S
  def dependencies: Set[Node[Stage,_]]
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

case class Wrap[Inner <: Stage, +S <: Stage, +T](stage: S, n: Node[Inner,T]) extends Node[S,T]{
  override def dependencies: Set[Node[Stage, _]] = Set(n)
}
case class App[+S <: Stage,+T](stage: S, inputs: IndexedSeq[Node[S,_]], f: IndexedSeq[_] => T, effort: Effort = Effort.low, name: Option[String] = None) extends Node[S,T]{
  override def dependencies: Set[Node[Stage, _]] = inputs.toSet
}
object App {
  def point[T](t: T): Node[Base.type, T] = App(Base, IndexedSeq(), _ => t, Effort.none)
  def map[S <: Stage, T, R](n: Node[S,T], effort: Effort = Effort.low)(f: T => R): Node[S,R] =
    App(n.stage, IndexedSeq(n), ins => f(ins(0).asInstanceOf[T]), effort)
  def map2[S1 <: Stage, S2 <: Stage, T1, T2, R](n1: Node[S1,T1], n2: Node[S2,T2], effort: Effort = Effort.low)(f: (T1,T2) => R)(implicit lub: StageLUB[S1,S2]): Node[lub.Out,R] =
    App(lub.lub, IndexedSeq(lub.lift1(n1),lub.lift2(n2)), (ins: IndexedSeq[_]) => f(ins(0).asInstanceOf[T1],ins(1).asInstanceOf[T2]))
}
case class Lift[+S <: Stage,+T](stage: S, p: Node[S,Stream[T]], perItemEffort: Effort = Effort.none, expectedLength: Length, name: Option[String] = None) extends Node[S,T] {
  override def dependencies: Set[Node[Stage, _]] = Set(p)
}
case class Report[+S <: Stage,+T, TT <: T](stage: S, n: Node[S,T], colName: String, f: TT => String) extends Node[S,T] {
  override def dependencies: Set[Node[Stage, _]] = Set(n)
}
case class Inject[+S <: Stage,+T] protected[node] (stage: S, p: S#Payload[T], name: Option[String] = None) extends Node[S,T] {
  override def dependencies: Set[Node[Stage, _]] = Set()
}

object Node{
  trait Staged[S <: Stage]{
    type N[+T] = Node[S,T]
  }

  def extractNodes(node: Node[Stage,_]): Set[Node[Stage,_]] =
    node.dependencies.flatMap(dn => extractNodes(dn) + dn) + node
}


