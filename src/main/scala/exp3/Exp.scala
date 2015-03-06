/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import shapeless.ops.hlist.{SubtypeUnifier, Comapped}
import shapeless.{::, HList, HNil}

import scala.language.reflectiveCalls

/** A node in the computation graph.
  * During one configuration, it holds a value of type `T`.
  * It produces some output columns in generated table. */
sealed trait Node[T]{
  /** The name of this node. */
  def name: String
  /** The columns produced by this node. */
  def columns: Seq[(String,T => String)] = Seq()
}

/** An InputNode has is a independent variable.
  * It has no dependencies and can be fed compatible values to trigger computations in the successor nodes. */
sealed trait InputNode[T] extends Node[T]

/** A computed node depends on some other nodes to compute it's value.
  * The computation might require resources. */
sealed trait ComputedNode[DN <: HList,D <: HList,T] extends Node[T] {
  implicit def unwrap: Comapped.Aux[DN,Node,D]
  def dependencies: DN
  def computation: D => T
}

case class IntP(name: String, default: Int, min: Int = Integer.MIN_VALUE, max: Int = Integer.MAX_VALUE) extends InputNode[Int]
case class LongP(name: String, default: Long, min: Long = Long.MinValue, max: Long = Long.MaxValue) extends InputNode[Long]
case class DoubleP(name: String, default: Double, min: Double = Double.NegativeInfinity, max:Double = Double.PositiveInfinity) extends InputNode[Double]
case class BoolP(name: String, default: Boolean) extends InputNode[Boolean]
//case class Sum[TND <: HList, T <: HList](name: String, alternatives: TND)(implicit unwrap: Comapped.Aux[TND,ND,T]) {
//  def fold[S,M <: HList](maps: M)(implicit unwrap: Comapped.Aux[M,({type l[X] = X=>S})#l,T]): Node[S] = ???
//}

case class Computation[DN <: HList, D <: HList, T](name: String, dependencies: DN)
                                                  (val computation: D => T)
                                                  (implicit val unwrap: Comapped.Aux[DN,Node,D]) extends ComputedNode[DN,D,T]

object Test {
  case class Problem(w: Int, s: Long)
  def main(args: Array[String]) {
    val width: Node[Int] = IntP("width", 2)
    val seed: Node[Long] = LongP("seed", 0)
    val problem = Computation("problem", width :: seed :: HNil)({
      x => Problem(x.head, x.tail.head)
    }: Int :: Long :: HNil => Problem)
  }
}