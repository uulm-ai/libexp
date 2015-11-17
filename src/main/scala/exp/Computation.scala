package exp

import shapeless._
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.{Mapped, ToTraversable}
import shapeless.ops.traversable.FromTraversable

/** An edge is the basic building block of a computation graph. It represents a typed, multi-ary function with a
  * stream-valued return type.
  */
case class Computation[+T] protected[Computation](name: String,
                                                  dependencies: IndexedSeq[Node[Any]],
                                                  f: IndexedSeq[Any] => T,
                                                  expectedLength: Double,
                                                  expectedCPU: Double) extends Edge[T] {

  override def computation: IndexedSeq[Any] => Stream[T] = f.andThen(Stream(_))
}

/**
  * Created by thomas on 17.11.15.
  */
object Computation {
  import shapeless.syntax.std.traversable._
  def apply[NHL <: HList, IHL <: HList, R, F](_name: String,
                                              expTime: Double)
                                             (deps: NHL)
                                             (f: F)
                                             ( implicit
                                               mapped: Mapped.Aux[IHL,Node,NHL],
                                               fn: FnToProduct.Aux[F,IHL => R],
                                               trN: ToTraversable.Aux[NHL, IndexedSeq, Node[Any]],
                                               trI: FromTraversable[IHL]
                                             )
  : Computation[R] = {
    val comp: IndexedSeq[Any] => R = (isa: IndexedSeq[Any]) => fn(f)(isa.toHList[IHL]
      .getOrElse(sys.error(s"error during conversion of '$isa' to hlist via '$trI' in node '${_name}'")))
    new Computation[R](_name, deps.to[IndexedSeq], comp, expectedLength = 1d, expectedCPU = expTime)
  }
}

case class LiftND[F](from: Node[Stream[F]], expectedLength: Double) extends Edge[F] {
  /** Has to be unique within the computation graph. */
  override def name: String = s"unwrap.${from.name}"
  override def expectedCPU: Double = from.expectedCPU
  def dependencies: IndexedSeq[Node[Any]] = IndexedSeq(from)
  override def computation: IndexedSeq[Any] => Stream[F] = _.head.asInstanceOf[Stream[F]]
}
