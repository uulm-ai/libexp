/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package libexp

import java.io.PrintStream

trait Exp[A]{
  type TRMap
  type TMap[_]
  type TFMap[_,_]

  def run: Iterable[Seq[String]]
  def names: Seq[String]
  def report(name: String, f: A => String = _.toString): TRMap
  def map[B](f: A => B): TMap[B]
  def flatMap[B,R](f: A => B)(implicit ev: TFMap[B,R]): R

  //implemented methods
  def printCSV(out: PrintStream = System.out): Unit = {
    out.println(names.mkString("\t"))
    run.map(_.mkString("\t")).foreach(out.println)
  }
}

object Exp{
  def empty: ParTup[_, _] = ParTup[Any,Any](Seq((null,null)),Report.empty)
  def seq[A](xs: Iterable[A]) = ParTup[A,Any](xs.view.map(x => (x,null)), Report.empty[(A,Any)])
  def mutableSeq[A](xs: Iterable[A]) = ParTup[A,Any](xs.view.map(x => (x,null)), Report.empty[(A,Any)])
}
