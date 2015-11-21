package exp.stages

import exp._

import scalaz._
import syntax.apply._

/** A Stage is a pre-processing step, which requires a value of type `Read` to process.
  * It wraps a type constructor `Inner` which must be an Apply. The resulting type `Stage#N[_]` is again
  * an Apply.
  *
  * @tparam Inner
  * @tparam T
  * @tparam Read
  */
trait Stage[Inner[+_],T,Read] {
  sealed trait N[+T]
  case class Wrap[+T](value: Inner[T]) extends N[T]
  case class MAp[A,+B](a: N[A], mapOrB: Either[A => B, N[A => B]]) extends N[B] {
    def transform[G[+_]](t: N ~> G)(implicit ap: Apply[G]): G[B] = mapOrB match {
      case Left(f) => ap.map(t(a))(f)
      case Right(nf) => ap.ap(t(a))(t(nf))
    }
  }
  /* Types specific to this stage shall extend the trait `Inject`. */
  trait Inject[+T] extends N[T]

  implicit def innerApply: Apply[Inner]

  def processInject(r: Read): Val[Inject ~> Inner]

  implicit def applyInstance: Apply[N] = new Apply[N]{
    override def ap[A, B](fa: => N[A])(f: => N[(A) => B]): N[B] = (fa,f) match {
      case (Wrap(fa),Wrap(f)) => Wrap(fa <*> f)
      case _                  => MAp(fa,Right(f))
    }
    override def map[A, B](fa: N[A])(f: (A) => B): N[B] = fa match {
      case Wrap(x) => Wrap(x map f)
      case _       => MAp(fa,Left(f))
    }
  }

  def process(r: Read): Val[N ~> Inner] = processInject(r).map{ injectHandler =>
    new ~>[N,Inner]{ poly =>
      override def apply[A](fa: N[A]): Inner[A] = fa match {
        case Wrap(x) => x
        case inj: Inject[A] => injectHandler(inj)
        case a: MAp[_,A] => a.transform(poly)
      }
    }
  }
}
