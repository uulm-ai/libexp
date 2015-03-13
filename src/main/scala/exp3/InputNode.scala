/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import org.parboiled2._

case class IntP(name: String, default: Int,
                min: Int = Integer.MIN_VALUE,
                max: Int = Integer.MAX_VALUE,
                fixed: Boolean = false) extends InputNode[Int] {
  override def parser(s: ParserInput): NDParser[Int] = new NDParser[Int] {
    override def input: ParserInput = s
    override def nd: Rule1[NonDeterminism[Int]] = rule { capture(Digits) ~> ((d: String) => Fixed(d.toInt)) }
  }
}
case class LongP(name: String,
                 default: Long,
                 min: Long = Long.MinValue,
                 max: Long = Long.MaxValue,
                 fixed: Boolean = false) extends InputNode[Long] {
  override def parser(s: ParserInput): NDParser[Long] = new NDParser[Long] {
    override def input: ParserInput = s
    override def nd: Rule1[NonDeterminism[Long]] = rule { capture(Digits) ~> ((d: String) => Fixed(d.toLong)) }
  }
}
//case class DoubleP(name: String,
//                   default: Double,
//                   min: Double = Double.NegativeInfinity,
//                   max:Double = Double.PositiveInfinity,
//                   fixed: Boolean = false) extends InputNode[Double]
//case class BoolP(name: String,
//                 default: Boolean,
//                 fixed: Boolean = false) extends InputNode[Boolean]
//case class Sum[TND <: HList, T <: HList](name: String, alternatives: TND)(implicit unwrap: Comapped.Aux[TND,ND,T]) {
//  def fold[S,M <: HList](maps: M)(implicit unwrap: Comapped.Aux[M,({type l[X] = X=>S})#l,T]): Node[S] = ???
//}