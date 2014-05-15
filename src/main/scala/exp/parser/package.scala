/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp

package object parser {
  //the following lines are stolen from Miles Sabin's shapeless library
  def unexpected : Nothing = sys.error("Unexpected invocation")
  trait <:!<[A, B]

  implicit def nsub[A, B] : A <:!< B = new <:!<[A, B] {}
  implicit def nsubAmbig1[A, B >: A] : A <:!< B = unexpected
  implicit def nsubAmbig2[A, B >: A] : A <:!< B = unexpected
  //shapeless code ends here

  /** Everything is an Exp. In some way or another.*/
  trait AsExp[A,B]{def apply(a: A): Exp[B]}
  //simply wrap anything that isn't an Exp or a parser tuple of exps
  implicit def asExpWrap[A](implicit ev: A <:!< Exp[_], ev2: A <:!< ExpParser#`~`[_,_]) = new AsExp[A,A]{ def apply(a: A): Exp[A] = Exp.values(a)}
  //every exp is an exp
  implicit def asExpId[A] = new AsExp[Exp[A],A]{def apply(a: Exp[A]): Exp[A] = a}
  //a parser tuple of exps is an exp of the cross-product of the contained exps
  implicit def pushTilde[P <: ExpParser,EA,EB,A,B]
  (implicit
    aAsExp: AsExp[EA,A],
    bAsExp: AsExp[EB,B]): AsExp[P#`~`[EA,EB],(A,B)] = new AsExp[P#`~`[EA,EB],(A,B)] {
    override def apply(p: P#`~`[EA, EB]): Exp[(A, B)] = for {
      a <- aAsExp(p._1)
      b <- bAsExp(p._2)
    } yield (a, b)
  }

  case class PExpT[P <: ExpParser,A](run: P#Parser[Exp[A]]){
    def map[B](f: A => B): PExpT[P,B] = PExpT(run.map(_.map(f)))
    def flatMap[B](f: A => PExpT[P,B]): PExpT[P,B] = ???
    def withFilter(p: A => Boolean): PExpT[P,A] = PExpT(run.map(_.withFilter(p)))
  }

  implicit class RichParser[P <: ExpParser,A](val p: P#Parser[A]) extends AnyVal {
    def lift[B](implicit asExp: AsExp[A,B]): PExpT[P,B] = PExpT(p.map(asExp.apply))
  }

  object ~~ {
    def unapply[A,B](l: (A,B)): Some[(A,B)] = Some(l)
  }

  implicit def unwrapExpT[P <: ExpParser,A](p: PExpT[P,A]): P#Parser[Exp[A]] = p.run
}
