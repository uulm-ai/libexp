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

case class IntP(name: String,
                default: NonDeterminism[Int],
                min: Int = Integer.MIN_VALUE,
                max: Int = Integer.MAX_VALUE) extends InputNode[Int] {
  override def parser(s: ParserInput): NDParser[Int] = new StratParser[Int] {
    override def input: ParserInput = s
    override def singleValue: Rule1[Int] = Int

    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = "whole number"
  }
}

case class LongP(name: String,
                 default: NonDeterminism[Long],
                 min: Long = Long.MinValue,
                 max: Long = Long.MaxValue) extends InputNode[Long] {
  override def parser(s: ParserInput): NDParser[Long] = new StratParser[Long] {
    override def input: ParserInput = s
    override def singleValue: Rule1[Long] = rule { capture(Digits) ~> ((_: String).toLong) }
    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = "whole number"
  }
}

case class Seed(name: String, default: Distribution[Long] = new Distribution[Long](r => r.nextLong()))
  extends InputNode[Long]{
  override val columns: Seq[(String, Long => String)] = Seq(name -> (_.toString))
  override def parser(s: ParserInput): NDParser[Long] = new NDParser[Long] {
    override def input: ParserInput = s
    override def nd: Rule1[NonDeterminism[Long]] = rule { capture(Digits) ~> ((s:String) => Fixed(s.toLong)) }
    override def syntaxDescription: String = s"provide a long to fix the random seed $name for all runs"
  }
}
case class DoubleP(name: String,
                   default: NonDeterminism[Double],
                   min: Double = Double.NegativeInfinity,
                   max:Double = Double.PositiveInfinity) extends InputNode[Double]{
  override def parser(s: ParserInput): NDParser[Double] = new StratParser[Double] {
    override def input: ParserInput = s
    override def singleValue: Rule1[Double] = Float
    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = s"floating point ($min to $max)"
  }
}
case class BoolP(name: String,
                 default: NonDeterminism[Boolean]) extends InputNode[Boolean] {
  override def parser(s: ParserInput): NDParser[Boolean] = new StratParser[Boolean] {
    override def input: ParserInput = s
    override def singleValue: Rule1[Boolean] = Bool
    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = s"boolean  ('true' and/or 'false)"
  }
}
//case class Sum[TND <: HList, T <: HList](name: String, alternatives: TND)(implicit unwrap: Comapped.Aux[TND,ND,T]) {
//  def fold[S,M <: HList](maps: M)(implicit unwrap: Comapped.Aux[M,({type l[X] = X=>S})#l,T]): Node[S] = ???
//}