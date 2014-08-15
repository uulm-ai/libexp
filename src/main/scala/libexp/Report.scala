/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package libexp

/** Represents a column-oriented report generator.
  * Contains column names, and functions mapping values to the table entries.
  *
  * @param columns For each column, the name and the function producing the entry.
  * @param naString The string to use when a table entry cannot be produced.
  * @tparam A The type for which the report can be produced. */
case class Report[-A](columns: Seq[(String,PartialFunction[A,String])], naString: String = "NA"){
  def names: Seq[String] = columns.map(_._1)
  def apply(a: A): Seq[String] = columns.map(_._2.applyOrElse(a, (_:A) => naString))
  def comap[B](f: PartialFunction[B,A]): Report[B] =
    Report(columns.map{case (n,g) => (n, Report.composePF(f,g))})
  def ++[O <: A](other: Report[O]): Report[O] = {
    require((other.names.toSet intersect names.toSet).isEmpty, "adding duplicate columns")
    Report(columns ++ other.columns)
  }
  def +[B <: A](x: (String, PartialFunction[B,String])): Report[B] = Report(columns :+ x)
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
