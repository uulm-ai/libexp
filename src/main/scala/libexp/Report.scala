/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package libexp

case class Report[A](columns: Seq[(String,PartialFunction[A,String])]){
  def names: Seq[String] = columns.map(_._1)
  def apply(a: A): Seq[String] = columns.map(_._2.applyOrElse(a, (_:A) => "NA"))
  def comap[B](f: PartialFunction[B,A]): Report[B] =
    Report(columns.map{case (n,g) => (n, Report.composePF(f,g))})
  def ++[O <: A](other: Report[O]): Report[O] = {
    require((other.names.toSet intersect names.toSet).isEmpty, "adding duplicate columns")
    Report(columns ++ other.columns)
  }
  def +(x: (String, PartialFunction[A,String])): Report[A] = Report(columns :+ x)
  def and[O](other: Report[O]): Report[(A,O)] =
    this.comap(PartialFunction((_:(A,O))._1)) ++ other.comap(PartialFunction(_._2))
  def or[O](other: Report[O]): Report[Either[A,O]] =
    this.comap[Either[A,O]]({case Left(a) => a}) ++ other.comap({case Right(b) => b})
}

object Report{
  def empty[A] = Report[A](Seq())
  def composePF[A,B,C](f1: PartialFunction[A,B], f2: PartialFunction[B,C]): PartialFunction[A,C] = new PartialFunction[A,C] {
    override def isDefinedAt(x: A): Boolean = f1.lift(x).exists(f2.isDefinedAt)
    override def apply(v1: A): C = f2(f1(v1))
  }
}
