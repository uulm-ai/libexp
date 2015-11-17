package exp

/** This type-class specifies a way to computation graphs that yield a specified node. */
trait Provided[+T] {
  def provide(name: String): Node[T]
}

/** Companion providing implicit instances. */
object Provided{
  def apply[T: Provided](name: String): Node[T] = implicitly[Provided[T]].provide(name)

  implicit def cliDouble: Provided[Double] = ???
  implicit def cliInt: Provided[Int] = ???
}
