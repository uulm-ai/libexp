package exp

import scala.language.higherKinds

import com.typesafe.scalalogging.StrictLogging
import fastparse.all._

import scalaz._
import scalaz.std.list._
import scalaz.syntax.traverse._

/** A node within the computation graph has a type `T` and a name. */
sealed trait Node[+T] { outer =>
  /** Has to be unique within the computation graph. */
  def name: String
  def expectedLength: Double
  def expectedCPU: Double
  def singleResult: Boolean

  def mapNamed[S](_name: String, f: T => S, expectedTime: Double = 0d): Edge[S] = Computation(
    name = _name,
    dependencies = IndexedSeq(outer),
    f = (ins: IndexedSeq[Any]) => f(ins.head.asInstanceOf[T]),
    expectedLength = outer.expectedLength,
    expectedCPU = outer.expectedCPU + expectedCPU
  )

  def lift[S](expectedLength: Double)(implicit ev: T <:< Stream[S]) = LiftND(outer.asInstanceOf[Node[Stream[S]]],expectedLength)
}

object Node {
  implicit val appInstance: Apply[Node] = new Apply[Node]{
    override def map[A, B](fa: Node[A])(f: (A) => B): Node[B] = fa.mapNamed(s"${fa.name}.$f",f,1d)

    override def ap[A, B](fa: => Node[A])(f: => Node[(A) => B]): Node[B] = new Computation[B](
      s"appy.${fa.name}.${f.name}",
      IndexedSeq(fa,f),
      (ins: IndexedSeq[Any]) => ins(1).asInstanceOf[A => B].apply(ins(0).asInstanceOf[A]),
      1d,
      0.001d
    )
  }
}

sealed trait Closed[+T] extends Node[T] {outer =>
  def closedDependencies: IndexedSeq[Closed[Any]]
}

trait ClosedEdge[+T] extends Edge[T] with Closed[T] {
  final def dependencies: IndexedSeq[Node[Any]] = closedDependencies
}

/** A seed node whose value is provided by the runtime. */
case object RNGSeed extends Closed[Long] {
  val closedDependencies: IndexedSeq[Closed[Any]] = IndexedSeq()
  /** Has to be unique within the computation graph. */
  val name: String = "RNGSeed"
  val expectedLength: Double = 1d
  val expectedCPU: Double = 0.0001d
  val singleResult: Boolean = true
}

/** This is a source node with fixed values. */
case class Fixed[+T](name: String, values: IndexedSeq[T]) extends ClosedEdge[T] {
  val closedDependencies: IndexedSeq[Closed[Any]] = IndexedSeq()
  val computation: IndexedSeq[Any] => Stream[T] = _ => values.toStream
  override def expectedLength: Double = values.size
  override def expectedCPU: Double = 0.00001d
  override def singleResult: Boolean = values.size == 1
}

object Fixed {
  def single[T](name: String, v: T): Fixed[T] = Fixed(name, IndexedSeq(v))
}

/** An input node without dependencies.
  * This node cannot be evaluated, it gets substituted with a different node at pre-processing time. */
case class FromCLI[+T](name: String,
                       valueParser: String => Val[Closed[T]],
                       default: Option[Closed[T]] = None,
                       description: String = "",
                       shortCommand: Option[Char] = None
                      ) extends Node[T] {
  def cliOpt: CliOpt[Closed[T]] = CliOpt[Closed[T]](name,valueParser,description,shortCommand,default)
  override def expectedLength: Double = Double.NaN
  override def expectedCPU: Double = Double.NaN
  override def singleResult: Boolean = false
}

object FromCLI extends StrictLogging {
  def ndParser[T](p: P[T]): P[Seq[T]] = (p.map(Seq(_)) | P("{" ~ p.rep(sep=P(",")) ~ "}")) ~ End

  def singleValue[T](name: String, p: String => Val[T], default: Option[T], desc: String, short: Option[Char] = None): FromCLI[T] =
    FromCLI(name, p andThen (_.map(Fixed.single(name, _))), default.map(Fixed.single(name,_)), desc, short)

  def multipleFromParser[T](name: String,
                            p: P[T],
                            default: Option[Seq[T]] = None,
                            desc: String = "",
                            short: Option[Char] = None): FromCLI[T] =
    FromCLI(
      name,
      CliOpt.parserToReader(ndParser(p).map(s =>  Fixed(name,s.toIndexedSeq))),
      default.map(s => Fixed(name,s.toIndexedSeq)),
      desc,
      short)
}

/** A node with dependencies, i.e., a computed node.
  * This edge may be used during graph construction, and it may have dependencies that are not [[exp.Closed]]. */
trait Edge[+T] extends Node[T]{ outer =>
  def dependencies: IndexedSeq[Node[Any]]
  def computation: IndexedSeq[Any] => Stream[T]
  def valuationStream(v: Valuation): Stream[Valuation] =
    computation(dependencies.map(v.apply[Any])).map(t => v + (this -> t))
  def close[M[+_]](r: Node[_] => M[Closed[_]])(implicit ap: Applicative[M]): M[ClosedEdge[T]] = {
    outer.dependencies.map(r).toList.sequence.map(closed =>
      new ClosedEdge[T] {
        override def computation: (IndexedSeq[Any]) => Stream[T] = outer.computation
        override def closedDependencies: IndexedSeq[Closed[Any]] = closed.toIndexedSeq
        override def expectedLength: Double = outer.expectedLength
        override def expectedCPU: Double = outer.expectedCPU
        /** Has to be unique within the computation graph. */
        override def name: String = outer.name

        override def hashCode(): Int = (name,closedDependencies,computation).hashCode()
        override def equals(obj: scala.Any): Boolean = obj match {
          case ce: ClosedEdge[T] => (ce.name,ce.closedDependencies,ce.computation) == (name, closedDependencies, computation)
          case _ => false
        }
        override def singleResult: Boolean = outer.singleResult
      }
    )
  }
}
