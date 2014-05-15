package exp

import shapeless._
import shapeless.TypeOperators._
import scala.util.parsing.combinator.Parsers
import shapeless.backports._

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 8/26/13
 */
package object parser {
  case class PExpT[P <: ExpParser,A](run: P#Parser[Exp[A]]){
    def map[B](f: A => B): PExpT[P,B] = PExpT(run.map(_.map(f)))
    def flatMap[B](f: A => PExpT[P,B]): PExpT[P,B] = ???
    def withFilter(p: A => Boolean): PExpT[P,A] = PExpT(run.map(_.withFilter(p)))
  }

//  implicit def unwrapPExpT[P <: ExpParser,A](pexpt: PExpT[P,A]): P#Parser[Exp[A]] = pexpt.run

  implicit class RichParser[P <: ExpParser,A](val p: P#Parser[A]) extends AnyVal{
    def lift[L <: HList](implicit ev: AsHEList[A,L]): P#Parser[Exp[L]] = p.map(ev.wrap)
    def liftExp[B](implicit ev: A <:< Exp[B]): PExpT[P, B] = PExpT(p.map(ev))
    def llE[L <: HList,B](implicit ev: AsHEList[A,L]): PExpT[P,L] = PExpT(p.map(ev.wrap))
  }

  /** Type-class that makes an `Exp` of anything. */
  trait AsExp[A,B]{def exp(a: A): Exp[B]}
  implicit def asExpWrap[A](implicit ev: A <:!< Exp[_]) = new AsExp[A,A]{ def exp(a: A): Exp[A] = Exp.values(a)}
  implicit def asExpId[A] = new AsExp[Exp[A],A]{def exp(a: Exp[A]): Exp[A] = a}

  trait AsHEList[T,HL <: HList]{
    def wrap: T => Exp[HL]
  }
  implicit def aHelPoint[A,B](implicit ev: AsExp[A,B], notCons: A <:!< Parsers#`~`[_,_]): AsHEList[A,B :: HNil] = new AsHEList[A,B :: HNil]{
    def wrap: A => Exp[B :: HNil] = (a: A) => ev.exp(a).map(_ :: HNil)
  }
  implicit def aHelConsParser[P <: ExpParser,A,B,AL <: HList,BL <: HList](implicit e1: AsHEList[A,AL],e2: AsHEList[B,BL], prepend: Prepend[AL,BL]): AsHEList[P#`~`[A,B],prepend.Out] =
    new AsHEList[P#`~`[A,B],prepend.Out]{
      def wrap: P#`~`[A,B] => Exp[prepend.Out] = x => for{
        a <- e1.wrap(x._1)
        b <- e2.wrap(x._2)
      } yield prepend(a,b)
    }

  //the following achieves the tilde extractor for HLists.
  //courtesy of Miles Sabin: http://stackoverflow.com/a/18484716/108915
  trait UnapplyLeft[L <: HList] extends DepFn1[L]

  trait LPUnapplyLeft {
    type Aux[L <: HList, Out0] = UnapplyLeft[L] { type Out = Out0 }
    implicit def unapplyHCons[L <: HList, I <: HList, F]
    (implicit
     init: InitAux[L, I],
     last: LastAux[L, F]): Aux[L, Option[(I, F)]] =
      new UnapplyLeft[L] {
        type Out = Option[(I, F)]
        def apply(l: L): Out = Option((l.init, l.last))
      }
  }

  object UnapplyLeft extends LPUnapplyLeft {
    implicit def unapplyPair[H1, H2]: Aux[H1 :: H2 :: HNil, Option[(H1, H2)]] =
      new UnapplyLeft[H1 :: H2 :: HNil] {
        type Out = Option[(H1, H2)]
        def apply(l: H1 :: H2 :: HNil): Out = Option((l.head, l.tail.head))
      }
  }

  object ~ {
    def unapply[L <: HList, Out](l: L)(implicit ua: UnapplyLeft.Aux[L, Out]): Out = ua(l)
  }
}
