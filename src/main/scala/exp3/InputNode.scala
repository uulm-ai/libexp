/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import org.parboiled2._

trait StratParser[T] extends NDParser[T]{
  final def syntaxDescription: String =
    """parses either a fixed value 'x', or a sequence enclosed by
      |curly brackets and delimited by commas; single values follow
      |the format: """.stripMargin + singleValueFormat

  final def nd: Rule1[NonDeterminism[T]] = rule { fixed | strat }
  final def strat: Rule1[Stratification[T]] = rule { '{' ~ (oneOrMore(singleValue).separatedBy(",") ~> (Stratification(_: Seq[T]))) ~ '}' }
  final def fixed: Rule1[Fixed[T]] = rule { singleValue ~> (Fixed(_: T)) }

  /** Parser that parses a single value. */
  def singleValue: Rule1[T]
  /** Describe syntax of specifying single values. */
  def singleValueFormat: String
}

case class IntP(name: String, default: Int,
                min: Int = Integer.MIN_VALUE,
                max: Int = Integer.MAX_VALUE,
                fixed: Boolean = false) extends InputNode[Int] {
  override def parser(s: ParserInput): NDParser[Int] = new StratParser[Int] {
    override def input: ParserInput = s
    override def singleValue: Rule1[Int] = rule { capture(Digits) ~> ((_: String).toInt) }

    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = "whole number"
  }
}

case class LongP(name: String,
                 default: Long,
                 min: Long = Long.MinValue,
                 max: Long = Long.MaxValue,
                 fixed: Boolean = false) extends InputNode[Long] {
  override def parser(s: ParserInput): NDParser[Long] = new StratParser[Long] {
    override def input: ParserInput = s
    override def singleValue: Rule1[Long] = rule { capture(Digits) ~> ((_: String).toLong) }
    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = "whole number"
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