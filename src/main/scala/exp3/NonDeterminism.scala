/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import scala.util.Random

/** Describes the kind of non-determinism to use for an input node of corresponding type. */
sealed trait NonDeterminism[+T]

object NonDeterminism {
  def fixed[T](x: T): Stratification[T] = Stratification(Seq(x))
  val empty = Stratification(Seq())
}

case class Stratification[+T](values: Seq[T]) extends NonDeterminism[T]{
  override def toString: String = s"{${values.mkString(",")}}"
}
case class Distribution[+T](sample: Random => T) extends NonDeterminism[T]