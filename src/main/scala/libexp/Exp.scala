/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package libexp

import java.io.PrintStream

trait Exp[A]{
  /** Return type after trivial modifications. */
  type TRet
  type TMap[_]
  type TFMap[_,_]

  def run: Iterable[Seq[String]]
  def addReport(r: (String, PartialFunction[A,String])): TRet
  def names: Seq[String]
  def map[B](f: A => B): TMap[B]
  def flatMap[B,R](f: A => B)(implicit ev: TFMap[B,R]): R

  //implemented methods
  def report(name: String, f: A => String = _.toString): TRet = addReport((name,PartialFunction(f)))
  def printCSV(out: PrintStream = System.out): Unit = {
    out.println(names.mkString("\t"))
    run.map(_.mkString("\t")).foreach(out.println)
  }
}