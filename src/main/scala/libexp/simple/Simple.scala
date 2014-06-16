/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package libexp.simple

import libexp.ND

/**
 * Created by thomas on 16.06.14.
 */
sealed trait Simple[E,T] extends ND[E,T]

object Simple{
  def parallel[A](xs: Iterable[A]): Simple[A,Unit] = ???
  def sequential[A](xs: Iterable[A]): Simple[A,Unit] = ???
}