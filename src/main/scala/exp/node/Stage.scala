package exp.node



trait Stage {
  type Payload[+T]
}

/** Type-class that indicates that stage `D` is layered below stage `H`, which means that
  * `H` is processed first, and `D` comes later. */
case class StageAfter[D <: Stage, H <: Stage](high: H){
  def wrap[T](n: Node[D,T]): Node[H,T] = Wrap(high,n)(this)
}

trait StageCast[F <: Stage, T <: Stage] {
  def toStage: T
  def apply[X](n: Node[F,X]): Node[T,X]
}

/** Contains the implicits for transitivity. */
trait LowestPrioritySC {
  implicit def fromInstance[S <: Stage](implicit ev: StageAfter[_,S]) = new StageCast[S,S]{
    override def toStage: S = ev.high
    override def apply[X](n: Node[S, X]): Node[S, X] = n
  }
}
trait SCLowPriorityImplicits extends LowestPrioritySC {
  implicit def transitiveInstance[S1 <: Stage, S2 <: Stage, S3 <: Stage](implicit s1tos2: StageCast[S1,S2], s1uneqs2: S1 =!= S2, s2tos3: StageCast[S2,S3], s2uneqs3: S2 =!= S3): StageCast[S1,S3] = new StageCast[S1,S3]{
    override def toStage: S3 = s2tos3.toStage
    override def apply[X](n: Node[S1, X]): Node[S3, X] = s2tos3(s1tos2(n))
  }
}

object StageCast extends SCLowPriorityImplicits {
  implicit def fromSBefore[E <: Stage, L <: Stage](implicit sb: StageAfter[E,L]): StageCast[E,L] = new StageCast[E,L]{
    override def toStage: L = sb.high
    override def apply[X](n: Node[E, X]): Node[L, X] = sb.wrap(n)
  }
}

trait StageLUB[S1 <: Stage, S2 <: Stage]{
  type Out <: Stage
  def lub: Out
  def lift1: StageCast[S1,Out]
  def lift2: StageCast[S2,Out]
}
object StageLUB {
  type Aux[S1 <: Stage, S2 <: Stage, S <: Stage] = StageLUB[S1,S2]{type Out = S}
  implicit def lubInstance[S1 <: Stage, S2 <: Stage, LUB <: Stage](implicit cs1: StageCast[S1,LUB], cs2: StageCast[S2,LUB]) = new StageLUB[S1,S2] {
    type Out = LUB
    override def lub: LUB = cs1.toStage
    override def lift1: StageCast[S1, LUB] = cs1
    override def lift2: StageCast[S2, LUB] = cs2
  }
}
