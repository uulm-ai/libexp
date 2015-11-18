package exp

import fastparse.all._

import scala.language.higherKinds
import scalaz._
import std.list._
import syntax.traverse._

/** A node within the computation graph has a type `T` and a name. */
sealed trait Node[+T] { outer =>
  /** Has to be unique within the computation graph. */
  def name: String
  def expectedLength: Double
  def expectedCPU: Double

  override final def hashCode(): Int = name.hashCode

  override final def equals(obj: scala.Any): Boolean = obj match {
    case e: Edge[_] => e.name == name
    case _ => false
  }

  override def toString: String = s"Node($name)"

  def map[S](_name: String, f: T => S, expectedTime: Double = 0d): Edge[S] = new Edge[S] {
    override def dependencies: IndexedSeq[Node[Any]] = IndexedSeq(outer)

    override def computation: (IndexedSeq[Any]) => Stream[S] = { ins =>
      Stream(f(ins.head.asInstanceOf[T]))
    }
    override def expectedLength: Double = outer.expectedLength
    override def expectedCPU: Double = outer.expectedCPU + expectedCPU
    /** Has to be unique within the computation graph. */
    override def name: String = _name
  }

  def lift[S](expectedLength: Double)(implicit ev: T <:< Stream[S]) = LiftND(outer.asInstanceOf[Node[Stream[S]]],expectedLength)
}

sealed trait Closed[+T] extends Node[T] {outer =>
  def closedDependencies: IndexedSeq[Closed[Any]]
}

trait ClosedEdge[+T] extends Edge[T] with Closed[T] {
  final def dependencies: IndexedSeq[Node[Any]] = closedDependencies
}

/** A seed node whose value is provided by the runtime. */
case object RNGSeed extends Closed[Long] {
  override def closedDependencies: IndexedSeq[Closed[Any]] = IndexedSeq()
  /** Has to be unique within the computation graph. */
  override def name: String = "RNGSeed"
  override def expectedLength: Double = 1d
  override def expectedCPU: Double = 0.0001d
}

/** This is a source node with fixed values. */
case class Fixed[+T](name: String, values: IndexedSeq[T]) extends ClosedEdge[T] {
  val closedDependencies: IndexedSeq[Closed[Any]] = IndexedSeq()
  val computation: IndexedSeq[Any] => Stream[T] = _ => values.toStream
  override def expectedLength: Double = values.size
  override def expectedCPU: Double = 0.00001d
}

/** An input node without dependencies.
  * This node cannot be evaluated, it gets substituted with a different node at preprocessing time. */
case class FromString[+T](name: String,
                          valueParser: P[Closed[T]],
                          default: Option[Closed[T]] = None,
                          description: String = "",
                          shortCommand: Option[Char] = None
                         ) extends Node[T] {
  def cliOpt: CliOpt[Closed[T]] = CliOpt(name, shortCommand, valueParser, default, description)
  def argNameParser = P(("--" ~ name) | ("-" ~ shortCommand.map(c => CharIn(Seq(c))).getOrElse(P(Fail))))
  override def expectedLength: Double = Double.NaN
  override def expectedCPU: Double = Double.NaN
}

/** A node with dependencies, i.e., a computed node.
  * This edge may be used during graph construction, and it may have dependencies that are not [[exp.Closed]]. */
trait Edge[+T] extends Node[T]{ outer =>
  def dependencies: IndexedSeq[Node[Any]]
  def computation: IndexedSeq[Any] => Stream[T]
  def valuationStream(v: Valuation): Stream[Valuation] =
    computation(dependencies.map(v.apply[Any](_))).map(t => v + (this -> t))
  def close[M[+_]](r: Node[_] => M[Closed[_]])(implicit ap: Applicative[M]): M[ClosedEdge[T]] = {
    outer.dependencies.map(r).toList.sequence.map(closed =>
      new ClosedEdge[T] {
        override def computation: (IndexedSeq[Any]) => Stream[T] = outer.computation
        override def closedDependencies: IndexedSeq[Closed[Any]] = closed.toIndexedSeq
        override def expectedLength: Double = outer.expectedLength
        override def expectedCPU: Double = outer.expectedCPU
        /** Has to be unique within the computation graph. */
        override def name: String = outer.name
      }
    )
  }
}
