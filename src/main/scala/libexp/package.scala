/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package object libexp {
  def empty: ParTup[_, _] = ParTup[Any,Any](Seq((null,null)),Report.empty)
  def seq[A](xs: Iterable[A]) = ParTup[A,Any](xs.view.map(x => (x,null)), Report.empty[(A,Any)])
  def mutableSeq[A](xs: Iterable[A]) = ParTup[A,Any](xs.view.map(x => (x,null)), Report.empty[(A,Any)])
}

