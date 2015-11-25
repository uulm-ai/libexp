package exp.stages

import exp.Val

import scalaz.{~>, Apply, Id}

/**
  * Created by thomas on 23.11.15.
  */
object Final extends Stage {
  override type Read = Unit

  type Inner[+T] = T
  case class FromSq[T](in: Seq[T], name: String) extends Inject[T]

  def fromSeqIns: FromSeq[N] = new FromSeq[N]{
    override def fromSeq[T](xs: Seq[T], name: String): Final.N[T] = FromSq(xs, name)
  }

  //the following method should never be called, maybe create a particular subtype of stage for final stages
  override implicit def innerLift: LiftStream[Id.Id] = ???
  override implicit def innerApply: Apply[Id.Id] = ???
  override def processInject(r: Unit, n: N[_]): Val[~>[Final.Inject, Id.Id]] = ???

  object ProvidedInstances {
    implicit val fromSeqInt: FromSeq[N] = fromSeqIns
  }
}
