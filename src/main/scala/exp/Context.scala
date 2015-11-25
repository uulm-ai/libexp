package exp

import exp.stages.LiftStream

import scala.language.higherKinds
import scala.reflect.ClassTag
import scalaz.{~>, Apply}
import scalaz.syntax.monoid._
import scalaz.syntax.apply._
import scalaz.std.map._
import scalaz.std.list._

trait Annotation

case class Name(name: String) extends Annotation
case class Report[-T: ClassTag](columnName: String, report: T => String) extends Annotation

trait AnnotationOps[+T,N[+_]] { self: Context[T,N] =>
    def withName(n: String): Context[T,N] =
      self.annotate(Name(n))
    def withReport[TT >: T](column: String, reporter: TT => String)(implicit ev: ClassTag[TT]): Context[T,N] =
      self.annotate(Report(column,reporter))
}

case class Context[+T,N[+_]](value: N[T], annotations: Map[N[_],List[Annotation]] = Map[N[_],List[Annotation]]()) extends AnnotationOps[T,N] {
  def annotate(a: Annotation): Context[T,N] = this.copy(annotations = annotations |+| Map(value -> List(a)))
}

object Context {
  type WithAny[tN[+_]] = Context[_,tN]

  /** Use `Context.Applied[N]#L` this if you want to partially apply the type-constructur N[+_] in the Context type. */
  trait Applied[N[+_]] {
    type L[+T] = Context[T,N]
  }

  implicit def applyInstance[N[+_]](implicit applyA: Apply[N])
  : Apply[Applied[N]#L] =
    new Apply[Applied[N]#L]{
      override def ap[A, B](fa: => Context[A,N])(f: => Context[A => B,N]): Context[B,N] =
        Context[B,N](fa.value <*> f.value, fa.annotations |+| f.annotations)
      override def map[A, B](fa: Context[A,N])(f: (A) => B): Context[B,N] = fa.copy(value = fa.value.map(f))
    }

  implicit def liftStreamInstance[N[+_]](implicit liftStreamN: LiftStream[N])
  : LiftStream[Applied[N]#L] =
    new LiftStream[Applied[N]#L]{
      override def liftStream[T](nst: Context[Stream[T],N]): Context[T,N] =
        nst.copy(value = liftStreamN.liftStream(nst.value))
    }

  implicit def wrapInstance[N[+_]]
  : N ~> Applied[N]#L =
    new ~>[N,Applied[N]#L]{
      override def apply[A](fa: N[A]): Context[A,N] = Context(fa)
    }

}