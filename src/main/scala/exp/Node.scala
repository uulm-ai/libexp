package exp

import com.typesafe.scalalogging.StrictLogging
import exp.stages.{Stage, RNGInsertion}
import exp.stages.evaluation.Evaluation

import scala.language.higherKinds

import scalaz.Apply

trait Node[+T] {
  /** Indicates which pre-processing steps are necessary for this node. */
  type S <: Stage
}

object Node {
  def applyInstance[S <: Stage]: Apply[S#N] = new Apply[S#N]{
    override def ap[A, B](fa: => S#N[A])(f: => S#N[(A) => B]): S#N[B] =
      Computed.dunno[S,B](Computed[S,B](IndexedSeq(fa,f),(in: IndexedSeq[_]) => Stream(in(1).asInstanceOf[A => B].apply(in(0).asInstanceOf[A]))))
    override def map[A, B](fa: S#N[A])(f: (A) => B): S#N[B] = ???
  }
  def fromSeq[T](values: Seq[T]) = Computed[Evaluation,T](IndexedSeq(), _ => values.toStream)
}




case object RNGSeed extends Node[Long]{
  type S = RNGInsertion.type
}

case class Computed[+TS <: Stage,+T](dependencies: IndexedSeq[TS#N[Any]], computation: IndexedSeq[_] => Stream[T])
  extends Node[T] {
  type S <: TS
}

object Computed {
  implicit def dunno[S <: Stage, T]: (Computed[S,T] <:< S#N[T]) = implicitly[Computed[S,T] <:< S#N[T]]
}

case class ComputationGraph[N[_] <: Node[_]](nodes: Set[N[_]])
