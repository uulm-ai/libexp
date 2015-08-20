/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3
import fastparse._

trait StratParser[T] extends NDParser[T]{
  final def syntaxDescription: String =
    """parses either a fixed value 'x', or a sequence enclosed by
      |curly brackets and delimited by commas; single values follow
      |the format: """.stripMargin + singleValueFormat

  final def nd: P[NonDeterminism[T]] = P(fixed | strat)
  final def strat: P[Stratification[T]] = P( "{" ~ singleValue.rep1(delimiter=",") ~ "}").map(Stratification(_))
  final def fixed: P[Fixed[T]] = singleValue.map(Fixed(_))

  /** Parser that parses a single value. */
  def singleValue: P[T]
  /** Describe syntax of specifying single values. */
  def singleValueFormat: String
}

case class IntP(name: String,
                default: NonDeterminism[Int],
                min: Int = Integer.MIN_VALUE,
                max: Int = Integer.MAX_VALUE) extends InputNode[Int] {
  override val parser: NDParser[Int] = new StratParser[Int] {
    override def singleValue: P[Int] = PrimitiveParsers.integer

    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = "whole number"
  }
}

case class LongP(name: String,
                 default: NonDeterminism[Long],
                 min: Long = Long.MinValue,
                 max: Long = Long.MaxValue) extends InputNode[Long] {
  override val parser: NDParser[Long] = new StratParser[Long] {
    override def singleValue: P[Long] = PrimitiveParsers.long
    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = "whole number"
  }
}

case class Seed(name: String, default: Distribution[Long] = new Distribution[Long](r => r.nextLong()))
  extends InputNode[Long]{
  override val columns: Seq[(String, Long => String)] = Seq(name -> (_.toString))
  override val parser: NDParser[Long] = new NDParser[Long] {
    override def nd: P[NonDeterminism[Long]] = PrimitiveParsers.long.map(Fixed(_))
    override def syntaxDescription: String = s"provide a long to fix the random seed $name for all runs"
  }
}
case class DoubleP(name: String,
                   default: NonDeterminism[Double],
                   min: Double = Double.NegativeInfinity,
                   max:Double = Double.PositiveInfinity) extends InputNode[Double]{
  override val parser: NDParser[Double] = new StratParser[Double] {
    override def singleValue: P[Double] = PrimitiveParsers.number
    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = s"floating point ($min to $max)"
  }
}
case class BoolP(name: String,
                 default: NonDeterminism[Boolean]) extends InputNode[Boolean] {
  override val parser: NDParser[Boolean] = new StratParser[Boolean] {
    override def singleValue: P[Boolean] = PrimitiveParsers.bool
    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = s"boolean  ('true' and/or 'false)"
  }
}

