package exp.computation

import java.io.{OutputStream, PrintStream}

import cats.Eval

/** Represents a group of outputs, i.e. a table, together with an output handler `eval`.
  */
trait Channel[T] {
  /** Name of this channel.*/
  def name: String
  /** The nodes for which values must be provided to `eval`. */
  def nodes: Set[CNode]
  /** A `Valuation` that is valid for at least the nodes in `nodes`. */
  def eval(values: Valuation, state: T): Eval[T]
}

trait UnsafeChannel extends Channel[Unit] {
  /** A `Valuation` that is valid for at least the nodes in `nodes`. */
  override def eval(values: Valuation, state: Unit): Eval[Unit] = evalUnsafe(values)
  def evalUnsafe(values: Valuation): Eval[Unit]
}
case class CSVWriter(name: String, columns: Seq[CColumn], out: OutputStream, sep: String = "\t") extends UnsafeChannel{
  val print = new PrintStream(out)

  /** The nodes for which values must be provided to `eval`. */
  override def nodes: Set[CNode] = columns.map(_.node)(collection.breakOut)

  def evalUnsafe(valuation: Valuation): Eval[Unit] = Eval.later{
    out synchronized {
      print.println(columns.map(c => c.f(valuation(c.node))).mkString(sep))
    }
  }
}
