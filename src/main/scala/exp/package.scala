import scala.language.implicitConversions

/**
  * Created by thomas on 18.11.15.
  */
package object exp {
  type Val[+T] = Either[String,T]

  implicit class RichT[T](val t: Val[T]) {
    def ensureNel(err: String, test: T => Boolean): Val[T] = t.filterOrElse(test,err)
  }

//  implicit def wrapInContext[T, N[_] <: Node[_]](n: N[T]): Context[N, T] = Context[N,T](n)

  def graphClosure[A](query: Iterable[A])(pred: A => Iterable[A]): Set[A] =
    Iterator.iterate(query.toSet)(found => found.flatMap(pred) ++ found)
      .sliding(2)
      .dropWhile(two => two(0).size != two(1).size)
      .next().head

  def topologicalOrder[A](query: Iterable[A])(pred: A => Iterable[A]): Seq[A] = {
    val allNodes = graphClosure(query)(pred)

    def topoSort(remaining: Set[A], acc: List[A], closed: Set[A]): List[A] =
      if(remaining.isEmpty) acc.reverse
      else {
        val next = remaining.find(cand => pred(cand).forall(closed)).getOrElse(sys.error("oops, possibly a cycle in DAG?"))
        topoSort(remaining - next, next :: acc, closed + next)
      }

    topoSort(allNodes, Nil, Set())
  }
}
