package exp

import shapeless._
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.{Mapped, ToTraversable}
import shapeless.syntax.typeable.typeableOps

/** An edge is the basic building block of a computation graph. It represents a typed, multi-ary function with a
  * stream-valued return type.
  */
case class Computation[+T] protected[Computation](name: String,
                                                  dependencies: IndexedSeq[Node[Any]],
                                                  f: IndexedSeq[Any] => T,
                                                  expectedLength: Double,
                                                  expectedCPU: Double) extends Edge[T] {

  override def computation: IndexedSeq[Any] => Stream[T] = f.andThen(Stream(_))
  override def hashCode(): Int = name.hashCode
  override def equals(obj: scala.Any): Boolean = obj match {
    case e: Computation[_] => e.name == name
    case _ => false
  }
}

/**
  * Created by thomas on 17.11.15.
  */
object Computation {
  def apply[NHL <: HList, IHL <: HList, R, F](_name: String,
                                              expTime: Double)
                                             (deps: NHL)
                                             (f: F)
                                             ( implicit
                                               mapped: Mapped.Aux[IHL,Node,NHL],
                                               fn: FnToProduct.Aux[F,IHL => R],
                                               trN: ToTraversable.Aux[NHL, IndexedSeq, Node[Any]],
                                               typeableI: Typeable[IHL]
                                             )
  : Computation[R] = {
    val comp: IndexedSeq[Any] => R = (isa: IndexedSeq[Any]) => fn(f)(isa.cast[IHL].getOrElse(sys.error(s"error during cast via $typeableI")))
    new Computation[R](_name, deps.to[IndexedSeq], comp, expectedLength = 1d, expectedCPU = expTime)
  }
}

case class LiftND[F](from: Edge[Stream[F]], expectedLength: Double) extends Edge[F] {
  /** Has to be unique within the computation graph. */
  override def name: String = s"unwrap.${from.name}"
  override def expectedCPU: Double = from.expectedCPU
  def dependencies: IndexedSeq[Node[Any]] = IndexedSeq(from)
  override def computation: IndexedSeq[Any] => Stream[F] = from.computation(_).flatten
}
