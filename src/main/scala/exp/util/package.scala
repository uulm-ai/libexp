package exp

import scala.annotation.tailrec

package object util {
  @tailrec
  def topoSort[T: HasDependencies: Ordering](remaining: Set[T], acc: List[T] = Nil): List[T] = {
    val open = remaining.filterNot(r => HasDependencies[T].dependencies(r).exists(remaining))
    if(open.isEmpty){
      require(remaining.isEmpty, "computation graph contains a directed cycle")
      acc.reverse
    } else {
      val n = open.min
      topoSort(remaining - n, n :: acc)
    }
  }

}
