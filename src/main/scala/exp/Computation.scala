package exp

import shapeless._
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.{Comapped, Mapped, ToTraversable}
import shapeless.ops.product.ToHList
import shapeless.ops.traversable.FromTraversable

/** An edge is the basic building block of a computation graph. It represents a typed, multi-ary function with a
  * stream-valued return type.
  */
case class Computation[+T] protected[exp](name: String,
                                          dependencies: IndexedSeq[Node[Any]],
                                          f: IndexedSeq[Any] => T,
                                          expectedLength: Double,
                                          expectedCPU: Double) extends Edge[T] {
  override def computation: IndexedSeq[Any] => Stream[T] = f.andThen(Stream(_))
  val singleResult: Boolean = true
}

/**
  * Created by thomas on 17.11.15.
  */
object Computation {
  trait CompTyper[-N, +R]{
    type F

    def nodesToSeq(nhl: N): IndexedSeq[Node[_]]
    def buildComputation(f: F): IndexedSeq[Any] => R
  }
  object CompTyper {
    import shapeless.syntax.std.product._

    type Aux[N, R, Fx] = CompTyper[N, R] {type F = Fx}

    implicit def shapelessProductInstance[In <: Product, N <: HList, I <: HList, R, Fx]
    (implicit
     toHl: ToHList.Aux[In,N],
     mapped: Comapped.Aux[N, Node, I],
     fn: FnToProduct.Aux[Fx, I => R],
     trN: ToTraversable.Aux[N, IndexedSeq, Node[Any]],
     trI: FromTraversable[I])
    : Aux[In, R, Fx] = new CompTyper[In, R] {
      override type F = Fx
      override def nodesToSeq(nhl: In): IndexedSeq[Node[_]] = trN(toHl(nhl))
      override def buildComputation(f: F): (IndexedSeq[Any]) => R = { (ia: IndexedSeq[Any]) =>
        fn(f)(trI.apply(ia).getOrElse(sys.error(s"error during conversion of '$ia' to hlist via '$trI'")))
      }
    }

    implicit def shapelessSingletonInstance[T, In <: Node[T], R]
    : Aux[In, R, T => R] = new CompTyper[In, R] {
      override type F = T => R
      override def nodesToSeq(nhl: In): IndexedSeq[Node[_]] = IndexedSeq(nhl)
      override def buildComputation(f: F): (IndexedSeq[Any]) => R = { (ia: IndexedSeq[Any]) =>
        f(ia.head.asInstanceOf[T])
      }
    }
  }

  def apply[N, R, F](name: String, expTime: Double = 0.001d)(deps: N)(f: F)(implicit ct: CompTyper.Aux[N,R,F]): Computation[R] =
    new Computation[R](name, ct.nodesToSeq(deps), ct.buildComputation(f), 1d, expTime)
}

case class LiftND[F](from: Node[Stream[F]], expectedLength: Double) extends Edge[F] {
  /** Has to be unique within the computation graph. */
  val name: String = s"unwrap.${from.name}"
  val expectedCPU: Double = from.expectedCPU
  val dependencies: IndexedSeq[Node[Any]] = IndexedSeq(from)
  override def computation: IndexedSeq[Any] => Stream[F] = _.head.asInstanceOf[Stream[F]]
  val singleResult: Boolean = false
}
