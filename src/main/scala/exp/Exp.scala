/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp

import reporter.Reporter
import Reporter._
import scala.collection.generic.FilterMonadic
import java.lang.management.ManagementFactory

/** The Experimentation Monad. */
trait Exp[B] { outer =>
  protected def generator: Iterable[(B,Reporter[B])]
  /** @return true, if calling `next` on an obtained iterator will destroy the last result. */
  def mutableIterator: Boolean = true

  def create: (Seq[String],Iterator[Seq[String]]) = {
    def createString(brb: (B,Reporter[B])) = brb._2.eval(brb._1)
    val it = generator.iterator
    val first = it.next()
    val header = first._2.colNames
    (header, Iterator(Iterator(createString(first)),it.map(createString)).flatten)
  }

  def map[C](f: B => C): Exp[C] = Exp(
    for{
      (b,rb) <- generator.view
      c = f(b)
    } yield (c,rb.hold(b): Reporter[C])
  )

  def withFilter(p: B => Boolean): Exp[B] = Exp(generator.view.withFilter(brb => p(brb._1)))

  def flatMap[C](f: B => Exp[C]): Exp[C] = Exp(
    for{
      (b,rb) <- outer.generator.view
      reportB = rb.hold(b)
      (c,rc) <- f(b).generator
    } yield (c, reportB + rc: Reporter[C])
  )
  def take(n: Int): Exp[B] = Exp(generator.view.take(n))
  def withReport(r: Reporter[B]) = Exp(generator.view.map{case (b,rb) => (b,rb also r)})
  def addColumn(name: String, value: B => String): Exp[B] = withReport(Reporter(name)(value))
  def reportAs(name: String): Exp[B] = addColumn(name, _.toString)

  /** Adds a column that measures the used computation time. Use with care! Double check results for plausibility!. */
  def reportCpuTimeAs(s: String): Exp[B] = Exp(
    new Iterable[(B,Reporter[B])]{
      override def iterator: Iterator[(B, Reporter[B])] = new Iterator[(B, Reporter[B])]{
        val it = generator.iterator
        var filterTime = 0d
        override def hasNext: Boolean = {
          val (r,d) = measure(it.hasNext)
          filterTime += d
          r
        }
        override def next(): (B, Reporter[B]) = {
          val ((result, reporter), duration) = measure(it.next())
          val totalDuration = duration + filterTime
          filterTime = 0
          (result,reporter + Reporter.constant(s,totalDuration.toString))
        }
        def measure[A](a: => A): (A, Double) = {
          val start = ManagementFactory.getThreadMXBean.getCurrentThreadCpuTime
          val result = a
          val duration = (ManagementFactory.getThreadMXBean.getCurrentThreadCpuTime - start) * 1e-9
          (result,duration)
        }
      }
    }
  )

  /** Currently only supports flatMap calls. */
  //TODO support map calls
  def parallel(chunkSize: Int): Exp[B] = new Exp[B]{ inner =>
    protected def generator: Iterable[(B, Reporter[B])] = outer.generator

    override def map[C](f: (B) => C): Exp[C] = ???
    override def withFilter(p: (B) => Boolean): Exp[B] = outer.withFilter(p).parallel(chunkSize)
    override def flatMap[C](f: (B) => Exp[C]): Exp[C] = Exp.fromIteratorWithR(generator.grouped(chunkSize)
      .flatMap{ chunk =>
      chunk.par.map{ case (b,rb) =>
        f(b).generator.view.map{case (c,rc) =>  (null.asInstanceOf[C],(rb.hold(b): Reporter[C]) also (rc.hold(c): Reporter[C]))}.toIndexedSeq
      }
    }.flatten
    )
  }

  /* The following is intended only for debugging. */
  /** Warning, this ignores the Reporter for now .*/
  override def equals(obj: scala.Any): Boolean = obj match {
    case eb: Exp[B] => (this.generator,eb.generator).zipped.forall{case (a,b) => a._1 == b._1}
    case _ => false
  }

  def force: (Seq[String], List[Seq[String]]) = create match {case (x,y) => (x,y.toList)}

  override def toString: String = {
    val (header, body) = create
    header.mkString("\t") + "\n" + body.map(_.mkString("\t")).mkString("\n")
  }
}

object Exp {
  def apply(): Exp[Unit] = Exp[Unit](Iterable((Unit,Reporter.empty)))
  def apply[A](it: Iterable[(A,Reporter[A])]): Exp[A] = new Exp[A]{
    protected def generator: Iterable[(A, Reporter[A])] = it.view
  }
  def fromIteratorWithR[A](it: => Iterator[(A,Reporter[A])]): Exp[A] = Exp(
    new Iterable[(A, Reporter[A])]{
      override def withFilter(p: ((A, Reporter[A])) => Boolean): FilterMonadic[(A, Reporter[A]), Iterable[(A, Reporter[A])]] = this
      def iterator: Iterator[(A, Reporter[A])] = it
    }
  )
  def fromIterable[A](i: Iterable[A]): Exp[A] = Exp(i.view.map(_ -> (Reporter.empty: Reporter[A])))
  def fromIterator[A](i: => Iterator[A]): Exp[A] = Exp.fromIterable(new Iterable[A]{
    def iterator: Iterator[A] = i
  })
  def values[A](vals: A*): Exp[A] = Exp(vals.map(a => a -> (Reporter.empty: Reporter[A])))
  def seed(start: Long, number: Int, name: String = "seed"): Exp[Long] =
    Exp.values(Seq.iterate(start,number)(_ + 1):_*)
      .addColumn(name,_.toString)

  implicit class ETuple2[A,B](val x: (Exp[A],Exp[B])) extends AnyVal {
    def xzipped: Exp[(A,B)] = for(a <- x._1; b <- x._2) yield (a,b)
  }
}