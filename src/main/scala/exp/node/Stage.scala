package exp.node

import scalaz._

trait Stage { outer =>
  type Payload[+T]
  type This <: Stage

  final type StageNode[+T] = Node[This,T]
  final type NextNode[+T]  = Node[Next,T]
  final type InjectNode[+T] = Inject[This,T]

  type Next <: Stage

  def nextStage: Next

  def runStage(injectProc: InjectNode ~> Next#StageNode): StageNode ~> NextNode =
    new ~>[StageNode,NextNode]{ inner =>
      override def apply[A](fa: StageNode[A]): NextNode[A] = fa match {
        case i: Inject[This,A] => injectProc(i).asInstanceOf[NextNode[A]]
        case Wrap(n) => n.asInstanceOf[Node[Next, A]] //validity of this cast is ensured by the existence of StageAfter values
        case App(ins, f, e, n) => App(ins.map(inner.apply[Any]), f, e, n)
        case Lift(p, pie, el, n) => Lift(inner.apply(p), pie, el, n)
        case Report(n, cn, f) => Report(inner.apply(n), cn, f.asInstanceOf[A => String])
        case otherwise => sys.error(s"inexhaustive match in CliProc on node $otherwise")
      }
    }
}

trait StageLess[L <: Stage, G <: Stage] {
  def apply[X](n: Node[L,X]): Node[G,X]
}

object StageLess {
  //unfortunately I could not get automatic inference of transitivity to work
  implicit def transitiveInstance: StageLess[Base.type,CliProc.type] =
    new StageLess[Base.type,CliProc.type]{
      override def apply[X](n: Node[Base.type, X]): Node[CliProc.type, X] =
        implicitly[StageLess[RngInsertion.type, CliProc.type]].apply(implicitly[StageLess[Base.type, RngInsertion.type]].apply(n))
    }

  implicit def stageOneStep[N <: Stage, S <: Stage {type Next = N}]: StageLess[N,S] =
    new StageLess[N,S]{
      override def apply[X](n: Node[N, X]): Node[S, X] = Wrap[S,X](n)
    }
//  implicit def transitiveStep[S1 <: Stage, S3 <: Stage, S2 <: Stage {type Next = S3}](implicit sl: StageLess[S2,S1]): StageLess[S3,S1] = ???
}

trait StageCast[From <: Stage, To <: Stage]{
  def apply[X](n: Node[From,X]): Node[To,X]
}

object StageCast {
  implicit def castEqual[S <: Stage]: StageCast[S, S] with Object {def apply[X](n: Node[S, X]): Node[S, X]} =
    new StageCast[S,S]{
      override def apply[X](n: Node[S, X]): Node[S, X] = n
    }
  implicit def upCast[Lower <: Stage, Upper <: Stage](implicit sl: StageLess[Lower,Upper]): StageCast[Lower, Upper] =
    new StageCast[Lower,Upper]{
      override def apply[X](n: Node[Lower, X]): Node[Upper, X] = sl(n)
    }
}

trait StageLUB[S1 <: Stage, S2 <: Stage]{
  type Out <: Stage
  def lift1: StageCast[S1,Out]
  def lift2: StageCast[S2,Out]
}

object StageLUB {
  type Aux[S1 <: Stage, S2 <: Stage, S <: Stage] = StageLUB[S1,S2]{type Out = S}
  implicit def lubInstance[S1 <: Stage, S2 <: Stage, LUB <: Stage](implicit cs1: StageCast[S1,LUB], cs2: StageCast[S2,LUB]) = new StageLUB[S1,S2] {
    type Out = LUB
    override def lift1: StageCast[S1, LUB] = cs1
    override def lift2: StageCast[S2, LUB] = cs2
  }
}