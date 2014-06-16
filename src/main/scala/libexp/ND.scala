/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package libexp

import java.io.PrintStream

/**
 *
 * @tparam E The exposed type.
 * @tparam T The complete type.
 */
trait ND[E,T] {
  def iterable: Iterable[(E,T)]
  def reporter: Report[(E,T)]

  def map[E2](f: E => E2): ND[E2,(E,T)]
  def flatMap[E2](f: E => Iterable[E2]): ND[E2,(E,T)]
  def withFilter(p: E => Boolean): ND[E,T]

  /** @return This and the given ND in sequence: */
  def append[T2,E2](other: ND[T2,E2]): ND[Either[E,E2],Either[T,T2]]

  def mapReporter(f: Report[(E,T)] => Report[(E,T)]): ND[E,T]

  //Implemented methods from here
  /** @return the cross product between this ND and another one. */
  def cross[T2,E2](other: ND[E2,T2]): ND[(E, E2), ((E, (E2, T2)), (E, T))] = {
    val zipped: ND[(E, (E2, T2)), (E, T)] = flatMap(e => other.iterable.map((e,_)))
    zipped
      .mapReporter(_ ++ other.reporter.comap(PartialFunction[((E, (E2, T2)), (E, T)), (E2,T2)]{
        case ((_,(e2,t2)),(_,_)) => (e2,t2)
      }))
      .map{case (e,(e2,_)) => (e,e2)}
  }

  /** @return An ND that reports the current value using its `toString` method. */
  def reportAs(name: String, by: E => String = _.toString): ND[E,T] =
    mapReporter(_ + (name -> PartialFunction(by compose ((_: (E, T))._1))))

  def writeCSV(out: PrintStream, sep: String = "\t"): Unit = {
    out.println(reporter.names.mkString(sep))
    iterable.foreach(x => out.println(reporter.apply(x).mkString(sep)))
  }
}