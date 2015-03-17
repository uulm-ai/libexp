/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import scala.util.Random

/** Describes the kind of non-determinism to use for an input node of corresponding type. */
sealed trait NonDeterminism[+T]

object NonDeterminism {
  def fixed[T](x: T): Stratification[T] = Stratification(Seq(x))
}

case class Fixed[+T](value: T) extends NonDeterminism[T]
case class Stratification[+T](values: Seq[T]) extends NonDeterminism[T]
case class Distribution[+T](sample: Random => T) extends NonDeterminism[T]