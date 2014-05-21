/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp2

/**
 * This trait encodes the properties of an experiment. The result of running an experiment is a series of
 * rows, each containing a string representation of the recorded properties.
 * Ex2HL implements Ex2 by maintaining the full value for each row. When requesting the row as string sequence,
 * all formatters will be applied.
 * @tparam A The type exposed by the Ex2 trait to apply logging to.
 * @tparam U The complete type of the experiment.
 */
sealed trait Ex2[A,U]{
  def iterable: Iterable[U]
  def reporter: Rep[U]
  def extract: U => A
  /**
   * @return For each value `A` a sequence of strings corresponding to to the values of the columns of the report.
   */
  def run: Iterator[Seq[String]]

  //build experiment using monadic operations
  def map[B](f: A => B): Ex2[B, (B,U)]
  def flatMap[B](f: A => Iterable[B]): Ex2[B,(B,U)]
  def withFilter(p: A => Boolean): Ex2[A,U]

  def zip[B,BU](other: Ex2[B,BU]): Ex2[(A,B),(U,BU)]

  /**
   * Extend the report by another column.
   * @param colName The column name.
   * @param f The formatter used for this column.
   * @return An experiment with the column added.
   */
  def log(colName: String, f: A => String): Ex2[A, U]

  //produce results
  /**
   * @return A list of column names.
   */
  def colNames: Seq[String] = reporter.columnNames

  def printCSV(): Unit = {
    println(colNames.mkString("\t"))
    run.map(_.mkString("\t")).foreach(println)
  }

  def cross[B,BU](other: Ex2[B,BU]): Ex2[(A,B),(U,BU)] = zip(other)

  /** @return The experiment logging the currently exposed value using its toString method. */
  def logAs(colName: String): Ex2[A, U] = log(colName, _.toString)
}

object Ex2 {
  /** Create a parallel experiment from the given iterable. */
  def values[A](values: A*): Ex2[A,A] = Ex2Par(Rep.empty[A],values.par, identity)
  /** Create a parallel experiment from the given iterable. */
  def apply[A](it: Iterable[A]): Ex2[A,A] = Ex2Par(Rep.empty,it.par,identity)

  /** Create an experiment with guaranteed sequential execution from a function producing an iterator.
    * Use this when you have any mutable state.
    * @return An Experiment that guarantees, that its trials will be evaluated sequentially.*/
  def sequential[A](it: () => Iterator [A]): Ex2[A,A] = Ex2Seq(Rep.empty,new Iterable[A]{def iterator = it()},identity)
}

private case class Ex2Par[H, U](reporter: Rep[U], pIterable: scala.collection.parallel.ParIterable[U], extract: U => H) extends Ex2[H, U]{
  def zip[B, BU](other: Ex2[B, BU]): Ex2[(H, B), (U, BU)] = other match {
    case Ex2Par(oRep,opIt,oExtract) => Ex2Par(
      reporter zip oRep,
      for(u <- pIterable.view; bu <- opIt.view) yield (u,bu),
      (hb: (U,BU)) => (extract(hb._1),oExtract(hb._2))
  )
    case Ex2Seq(oRep,opIt,oExtract) => Ex2Par(
      reporter zip oRep,
      for(u <- pIterable.view; bu <- opIt.view) yield (u,bu),
      (hb: (U,BU)) => (extract(hb._1),oExtract(hb._2))
    )
  }

  //build experiment using monadic operations
  def map[B](f: (H) => B): Ex2[B, (B,U)] = Ex2Par[B,(B,U)](reporter.comap(_._2),pIterable.view.map(u => (f(extract(u)),u)),_._1)

  def withFilter(p: (H) => Boolean): Ex2[H, U] = copy(pIterable= pIterable.view.filter(p compose extract))

  /**
   * Extend the report by another column.
   * @param colName The column name.
   * @param f The formatter used for this column.
   * @return An experiment with the column added.
   */
  def log(colName: String, f: (H) => String): Ex2[H, U] = copy(reporter = reporter.addColumn(colName,f compose extract))

  def run: Iterator[Seq[String]] = pIterable.view.map(reporter).force.iterator

  def iterable: Iterable[U] = pIterable.seq

  def flatMap[B](f: (H) => Iterable[B]): Ex2[B, (B, U)] = Ex2Par(
    reporter.comap(_._2),
    for(u <- pIterable.view; b <- f(extract(u)).view) yield(b,u),
    _._1
  )
}

private case class Ex2Seq[H, U](reporter: Rep[U], iterable: Iterable[U], extract: U => H) extends Ex2[H, U]{
  def zip[B, BU](other: Ex2[B, BU]): Ex2[(H, B), (U, BU)] = other match {
    case Ex2Par(oRep,opIt,oExtract) => Ex2Par(
      reporter zip oRep,
      for(bu <- opIt.view; u <- iterable.view) yield (u,bu), //make sure flatMap is called on the parallel collection
      (hb: (U,BU)) => (extract(hb._1),oExtract(hb._2))
    )
    case Ex2Seq(oRep,opIt,oExtract) => Ex2Seq(
      reporter zip oRep,
      for(u <- iterable.view; bu <- opIt.view) yield (u,bu),
      (hb: (U,BU)) => (extract(hb._1),oExtract(hb._2))
    )
  }

  //build experiment using monadic operations
  def map[B](f: (H) => B): Ex2[B, (B,U)] = Ex2Seq[B,(B,U)](reporter.comap(_._2),iterable.view.map(u => (f(extract(u)),u)),_._1)

  def withFilter(p: (H) => Boolean): Ex2[H, U] = copy(iterable= iterable.filter(p compose extract))

  /**
   * Extend the report by another column.
   * @param colName The column name.
   * @param f The formatter used for this column.
   * @return An experiment with the column added.
   */
  def log(colName: String, f: (H) => String): Ex2[H, U] = copy(reporter = reporter.addColumn(colName,f compose extract))

  def run: Iterator[Seq[String]] = iterable.map(reporter).iterator

  def flatMap[B](f: (H) => Iterable[B]): Ex2[B, (B, U)] = Ex2Seq(
    reporter.comap(_._2),
    for(u <- iterable; b <- f(extract(u))) yield (b,u),
    _._1
  )
}
