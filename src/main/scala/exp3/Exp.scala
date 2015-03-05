/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import shapeless.ops.hlist.Comapped
import shapeless.{::, HList, HNil}

import scala.language.reflectiveCalls


sealed trait Node[T]{
  def name: String
  def reportAs(colName: String, proc: T => String) = ???
}

sealed trait ND[T] extends Node[T]

case class IntP(name: String, default: Int, min: Int = Integer.MIN_VALUE, max: Int = Integer.MAX_VALUE) extends ND[Int]
case class LongP(name: String, default: Long, min: Long = Long.MinValue, max: Long = Long.MaxValue) extends ND[Long]
case class DoubleP(name: String, default: Double, min: Double = Double.NegativeInfinity, max:Double = Double.PositiveInfinity) extends ND[Double]
case class BoolP(name: String, default: Boolean) extends ND[Boolean]
case class Sum[TND <: HList, T <: HList](name: String, alternatives: TND)(implicit unwrap: Comapped.Aux[TND,ND,T]) {
  def fold[S,M <: HList](maps: M)(implicit unwrap: Comapped.Aux[M,({type l[X] = X=>S})#l,T]): Node[S] = ???
}

case class Computation[NDArgs <: HList, Args <: HList, S](name: String, args: NDArgs)
                                                         (payload: Args => S)
                                                         (implicit unwrap: Comapped.Aux[NDArgs,Node,Args]) extends Node[S]

object Test {
  case class Problem(w: Int, s: Long)
  def main(args: Array[String]) {
    val width: Node[Int] = IntP("width", 2)
    val seed: Node[Long] = LongP("seed", 0)
    val problem = Computation("problem", width :: seed :: HNil)({
      x => Problem(x.head, x.tail.head)
    }: Int :: Long :: HNil => Problem)
      .reportAs("exact", _.w.toString)
  }
}