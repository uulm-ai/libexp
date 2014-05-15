/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp2

/**
 * A report can map an individual of type `L` onto a series of statistics. It also provides the name of these statistics.
 * @tparam U The currently exposed type.
 */
case class Rep[U](columnNames: Seq[String], reports: Seq[U => String]) extends (U => Seq[String]){
  def apply(x: U): Seq[String] = reports.map(f => f(x))
  def addColumn(name: String, f: U => String): Rep[U] = Rep(columnNames :+ name, reports :+ f)
  def comap[B](f: B => U): Rep[B] = Rep(columnNames,reports.map(r => (x: B) => r(f(x))))
  def ++[O <: U](other: Rep[O]): Rep[O] = Rep(columnNames ++ other.columnNames, reports ++ other.reports)
  def zip[O](other: Rep[O]): Rep[(U,O)] = Rep(
    columnNames ++ other.columnNames,
    reports.map(r => (uo: (U,O)) => r(uo._1)) ++ other.reports.map(r => (uo: (U,O)) => r(uo._2))
  )
}

object Rep{
  def empty[A] = Rep[A](Seq(),Seq())
}
