/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import fastparse.all._
import fastparse.parsers.Terminals.Literal

trait StratParser[T] extends NDParser[T]{
  final def syntaxDescription: String =
    """parses either a fixed value 'x', or a sequence enclosed by
      |curly brackets and delimited by commas; single values follow
      |the format: """.stripMargin + singleValueFormat

  final def nd: P[NonDeterminism[T]] = P(fixedParser | strat)
  final def strat: P[Stratification[T]] = P( "{" ~ singleValue.rep(sep=",",min=1) ~ "}").map(Stratification(_))
  final def fixedParser: P[Stratification[T]] = singleValue.map(only(_))

  /** Parser that parses a single value. */
  def singleValue: P[T]
  /** Describe syntax of specifying single values. */
  def singleValueFormat: String
}


case class NDInput[A: NDParser](name: String, default: NonDeterminism[A]) extends InputNode[A] {
  override def parser: NDParser[A] = implicitly[NDParser[A]]
}

case class FixedInput[A: P](name: String, defaultValue: Option[A], report: Boolean = true) extends InputNode[A] {

  override def default: NonDeterminism[A] = defaultValue.map(only).getOrElse(NonDeterminism.empty)

  override def parser: NDParser[A] = new NDParser[A] {
    override def nd: P[NonDeterminism[A]] = implicitly[Parser[A]].map(only)
    override def syntaxDescription: String = "just a value"
  }

  override val columns: Seq[(String, (A) => String)] = if(report) Seq(name -> (_.toString)) else Seq()
}

case class EnumP(name: String, values: Set[String], default: NonDeterminism[String]) extends InputNode[String]{
  override def parser: NDParser[String] = new StratParser[String] {
    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = s"one of ${values.mkString(",")}; default is $default"

    /** Parser that parses a single value. */
    override def singleValue: P[String] = values.toSeq.sorted.map(Literal(_)).foldLeft[Parser[Unit]](Fail){case (pp,p) => pp | p}.!
  }
}

case class TypedEnumP[A](name: String, values: Map[String,A], default: NonDeterminism[A]) extends InputNode[A] {
  override def parser: NDParser[A] = new StratParser[A] {
    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = s"one of ${values.keys.mkString(",")}; default is $default"
    /** Parser that parses a single value. */
    override def singleValue: P[A] = values.keys.toSeq.sorted.map(Literal(_)).foldLeft[Parser[Unit]](Fail){case (pp,p) => pp | p}.!.map(values)
  }

  override val columns: Seq[(String, (A) => String)] = Seq(name -> (a => values.find(_._2 == a).map(_._1).getOrElse("oops")))
}

case class StringP(name: String, pattern: P[String] = CharsWhile(c => !Set(' ',',','{','}')(c),min=1).!) extends InputNode[String]{
  override def default: NonDeterminism[String] = NonDeterminism.empty

  override def parser: NDParser[String] = new StratParser[String]{
    /** Parser that parses a single value. */
    override def singleValue: P[String] = pattern

    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = "a string not containing any of ' ',',','{','}'"
  }
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
    override def nd: P[NonDeterminism[Long]] = PrimitiveParsers.long.map(only(_))
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

