import scala.language.implicitConversions
import scalaz.{NonEmptyList, ValidationNel}

/**
  * Created by thomas on 18.11.15.
  */
package object exp {
  type Val[+T] = ValidationNel[String,T]

  implicit class RichT[T](val t: Val[T]) {
    def ensureNel(err: String, test: T => Boolean): Val[T] = t.ensure(NonEmptyList(err))(test)
  }

//  implicit def wrapInContext[T, N[_] <: Node[_]](n: N[T]): Context[N, T] = Context[N,T](n)

  def graphClosure[A](query: Iterable[A])(pred: A => Iterable[A]): Set[A] =
    Iterator.iterate(query.toSet)(found => found.flatMap(pred) ++ found)
      .sliding(2)
      .dropWhile(two => two(0).size != two(1).size)
      .next().head
}
