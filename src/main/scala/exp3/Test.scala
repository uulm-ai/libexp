/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3


import shapeless.HList._
import shapeless.{:: => :::, HNil}


object Test {
  case class Problem(w: Int, s: Long)
  def main(args: Array[String]) {
    val width: ValuedNode[Int] = IntP("width", Fixed(2))
    val seed: ValuedNode[Long] = Seed("seed.problem")
    val problem = Computation("problem", width :: seed :: HNil)({
      x => Problem(x.head, x.tail.head)
    }: Int ::: Long ::: HNil => Problem)
      .report("foo", p => (p.s + p.w).toString)

    Experiment.run(Set(problem), args)
  }
}
