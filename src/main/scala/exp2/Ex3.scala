/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp2

/**
 * Created by thomas on 19.05.14.
 */
trait Ex3[F,T,E] {
  def iterable: F => Iterable[T]
  def reporter: Rep[T]
  def extract: T => E
}