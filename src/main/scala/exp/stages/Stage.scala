package exp.stages

import exp._
import exp.cli.CliOpt

import scalaz._
import syntax.apply._

/** A Stage is a pre-processing step, which requires a value of type `Read` to process.
  * It wraps a type constructor `Inner` which must be an Apply. The resulting type `Stage#N[_]` is again
  * an Apply.
  */
trait Stage {
  type Read
  type Inner[+_]
  sealed trait N[+_]
  case class Wrap[+T](value: Inner[T]) extends N[T]
  case class MAp[A,+B](a: N[A], mapOrB: Either[A => B, N[A => B]]) extends N[B] {
    def transform[G[+_]](t: N ~> G)(implicit ap: Apply[G]): G[B] = mapOrB match {
      case Left(f) => ap.map(t(a))(f)
      case Right(nf) => ap.ap(t(a))(t(nf))
    }
  }
  case class LiftStr[+T](a: N[Stream[T]]) extends N[T]

  /* Types specific to this stage shall extend the trait `Inject`. */
  trait Inject[+T] extends N[T]

  implicit def innerApply: Apply[Inner]

  implicit def innerLift: LiftStream[Inner]

  def processInject(r: Read, n: N[_]): Val[Inject ~> Inner]

  implicit def applyInstance: Apply[N] = new Apply[N]{
    override def ap[A, B](fa: => N[A])(f: => N[(A) => B]): N[B] = (fa,f) match {
      case (Wrap(xa),Wrap(x)) => Wrap(xa <*> x)
      case _                  => MAp(fa,Right(f))
    }
    override def map[A, B](fa: N[A])(f: (A) => B): N[B] = fa match {
      case Wrap(x) => Wrap(x map f)
      case _       => MAp(fa,Left(f))
    }
  }

  implicit def liftStreamInst: LiftStream[N] = new LiftStream[N]{
    override def liftStream[T](nst: N[Stream[T]]): N[T] = LiftStr(nst)
  }

  implicit def liftTransformation: Inner ~> N = new ~>[Inner,N]{
    override def apply[A](fa: Inner[A]): N[A] = Wrap(fa)
  }

  implicit def unwrap: Wrap ~> Inner = new ~>[Wrap,Inner]{
    override def apply[A](fa: Wrap[A]): Inner[A] = fa.value
  }

  def process[T](r: Read, n: N[_]): Val[N ~> Inner] = processInject(r,n).map{ injectHandler =>
    new ~>[N,Inner]{ poly =>
      override def apply[A](fa: N[A]): Inner[A] = fa match {
        case Wrap(x) => x
        case inj: Inject[A] => injectHandler(inj)
        case LiftStr(na) => innerLift.liftStream(poly(na))
        case a: MAp[_,A] => a.transform(poly)
      }
    }
  }
}

trait LiftStream[N[+_]]{
  def liftStream[T](nst: N[Stream[T]]): N[T]
}

trait FromSeq[+N[+_]]{outer =>
  def fromSeq[T](xs: Seq[T]): N[T]
}

object FromSeq{
  def liftInstance[N[+_],G[+_]](implicit fsn: FromSeq[N], ev: N ~> G) = new FromSeq[G]{
    override def fromSeq[T](xs: Seq[T]): G[T] = ev(fsn.fromSeq(xs))
  }
}

trait GetSeed[N[+_]]{
  def getSeed(name: String): N[Long]
}

object GetSeed{
  def liftInstance[N[+_],G[+_]](implicit gsn: GetSeed[N], trans: N ~> G) = new GetSeed[G]{
    override def getSeed(name: String): G[Long] = trans(gsn.getSeed(name))
  }
}
trait CLINode[N[+_],G[+_]]{
  def cliNode[T](co: CliOpt[G[T]]): N[T]
}
