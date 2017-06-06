package exp.computation

import java.io.{OutputStream, PrintStream}

import cats.Eval

trait ActionAny {
  /** Name of this channel.*/
  def name: String
  /** The nodes for which values must be provided to `eval`. */
  def dependencies: Set[CNode]
  /** Run the action only when all non-dependency nodes are assigned their first assignment. */
  def onlyOnFirst: Boolean = true

  def initialUT: Any
  def evalUT(values: Valuation, state: Any): Eval[Any]
}
/** Represents a group of outputs, i.e. a table, together with an output handler `eval`.
  */
trait Action[T] extends ActionAny {

  /** A `Valuation` that is valid for at least the nodes in `nodes`. */
  def eval(values: Valuation, state: T): Eval[T]
  /** Initial value. */
  def initial: T

  override def initialUT: Any = initial
  override def evalUT(values: Valuation, state: Any): Eval[Any] = eval(values,state.asInstanceOf[T])
}

trait UnsafeAction extends Action[Unit] {
  def evalUnsafe(values: Valuation): Eval[Unit]

  /** A `Valuation` that is valid for at least the nodes in `nodes`. */
  override def eval(values: Valuation, state: Unit): Eval[Unit] = evalUnsafe(values)

  /** Initial value. */
  override def initial: Unit = Unit
}
case class CSVWriter(name: String, columns: Seq[CColumn], out: OutputStream, sep: String = "\t") extends UnsafeAction{

  override def onlyOnFirst: Boolean = true

  val print = new PrintStream(out)

  /** The nodes for which values must be provided to `eval`. */
  override def dependencies: Set[CNode] = columns.map(_.node)(collection.breakOut)

  def evalUnsafe(valuation: Valuation): Eval[Unit] = Eval.later{
    out synchronized {
      print.println(columns.map(c => c.f(valuation(c.node))).mkString(sep))
    }
  }
}
