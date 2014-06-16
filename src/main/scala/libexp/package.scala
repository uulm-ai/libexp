/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

import libexp.simple.Simple

import scala.util.Random

package object libexp {
  type NDImpl[E,T] = Simple[E,T]
  def sequential[A](xs: Seq[A]): ND[A,Unit] = Simple.sequential(xs)
  def parallel[A](xs: Seq[A]): ND[A,Unit] = Simple.parallel(xs)

  /** @return A parallel ND[Long,_] over the given range. */
  def seed(n: Long, start: Long, name: Option[String] = None): ND[Long,Unit] = {
    val r = parallel(start until (start + n))
    name.map(r.reportAs(_)).getOrElse(r)
  }
  def randSeed(n: Long, start: Long, name: Option[String] = None): ND[Random, (Long, Unit)] =
    seed(n,start,name).map(new Random(_))

  implicit class RichND[E,T](val nd: ND[E,T]) extends AnyVal {
    def expand[E2](f: E => Iterable[E2]) = nd cross sequential()
  }
}

