package exp.stages.evaluation

import exp.Node
import exp.stages.Stage
import scalaz.Apply

/** Supplies [[scalaz.Apply]] functionality to [[exp.Node]]. */
case class Computed[+TS <: Stage,+T](dependencies: IndexedSeq[TS#N[Any]], computation: IndexedSeq[_] => Stream[T])
  extends Node[T] {
  type S <: TS
}

object Computed {
  implicit def applyInstance[S <: Stage]: Apply[S#N] = new Apply[S#N]{
    override def ap[A, B](fa: => S#N[A])(f: => S#N[(A) => B]): S#N[B] =
      Computed[S,B](IndexedSeq(fa,f),(in: IndexedSeq[_]) => Stream(in(1).asInstanceOf[A => B].apply(in(0).asInstanceOf[A])))
    override def map[A, B](fa: S#N[A])(f: (A) => B): S#N[B] =
      Computed[S,B](IndexedSeq(fa), (in: IndexedSeq[_]) => Stream(f(in(0).asInstanceOf[A])))
  }

  implicit def foo[S <: Stage,T]: (Computed[S,T] <:< S#N[T]) = implicitly[Computed[S,T] <:< S#N[T]]
}

trait ComputedOps {
  def fromSeq[T](values: Seq[T]): Computed[Evaluation,T] = Computed[Evaluation,T](IndexedSeq(), _ => values.toStream)
}
