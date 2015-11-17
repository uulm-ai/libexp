package exp

import scalaz.Validation

/** A node within the computation graph has a type `T` and a name. */
sealed trait Node[+T] {
  /** Has to be unique within the computation graph. */
  def name: String
}

/** An input node without dependencies. */
case class FromString[+T](name: String,
                          parser: String => Validation[String,Stream[T]],
                          default: Option[Stream[T]] = None,
                          description: String = "",
                          shortCommand: Option[Char] = None
                         ) extends Node[T]

/** A seed node whose value is provided by the runtime. */
case object RNGSeed extends Node[Long] {
  /** Has to be unique within the computation graph. */
  override def name: String = "RNGSeed"
}


/** A node with dependencies, i.e., a computed node. */
trait Edge[+T] extends Node[T]{
  def dependencies: IndexedSeq[Node[Any]]
  def computation: IndexedSeq[Any] => Stream[T]
  def expectedLength: Double
  def expectedCPU: Double
  def valuationStream(v: Valuation): Stream[Valuation] =
    computation(dependencies.map(v.apply[Any](_))).map(t => v + (this -> t))
}
