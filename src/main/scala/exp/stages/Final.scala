package exp.stages

import exp.Val

import scalaz.{~>, Apply, Id}

/**
  * Created by thomas on 23.11.15.
  */
object Final extends Stage {
  override type Read = Unit

  type Inner[+T] = T
  case class FromSq[T](in: Seq[T]) extends Inject[T]

  implicit def liftStreamInstance: LiftStream[N] = new LiftStream[N]{
    override def liftStream[T](nst: N[Stream[T]]): N[T] = LiftStr(nst)
  }

  implicit def fromSeqIns: FromSeq[N] = new FromSeq[N]{
    override def fromSeq[T](xs: Seq[T]): Final.N[T] = FromSq(xs)
  }

  //the following method should never be called, maybe create a particular subtype of stage for final stages
  override implicit def innerLift: LiftStream[Id.Id] = ???
  override implicit def innerApply: Apply[Id.Id] = ???
  override def processInject(r: Unit): Val[~>[Final.Inject, Id.Id]] = ???
}
