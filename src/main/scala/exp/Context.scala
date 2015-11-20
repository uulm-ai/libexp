package exp

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
    def withName(n: String)(implicit ev: N[_] <:< Node[_]): Context[T,N] =
      self.annotate(Name(n))
    def withReport[TT >: T](column: String, reporter: TT => String)(implicit ev: ClassTag[TT]): Context[T,N] =
      self.annotate(Report(column,reporter))
}

case class Context[+T,N[+_]](value: N[T], annotations: Map[N[_],List[Annotation]] = Map[N[_],List[Annotation]]()) extends AnnotationOps[T,N] {
  def map[S](f: T => S)(implicit ev: Apply[N]): Context[S,N] = Context[S,N](ev.map(value)(f), annotations)
  def annotate(a: Annotation): Context[T,N] = this.copy(annotations = annotations |+| Map(value -> List(a)))
}

trait ContextOps {
  implicit def addContext[T,N[+_]](nt: N[T]): Context[T,N] = Context(nt)
}

object Context {
  type WithAny[tN[+_]] = Context[_,tN]

  trait Foo[N[+_]] {
    type L[+T] = Context[T,N]
  }

  implicit def applyInstance[N[+_]](implicit applyA: Apply[N]): Apply[Foo[N]#L] = new Apply[Foo[N]#L]{
    override def ap[A, B](fa: => Context[A,N])(f: => Context[A => B,N]): Context[B,N] =
      Context[B,N](fa.value <*> f.value, fa.annotations |+| f.annotations)
    override def map[A, B](fa: Context[A,N])(f: (A) => B): Context[B,N] = fa.map(f)
  }
}
