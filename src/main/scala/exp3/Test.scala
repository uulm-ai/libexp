/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

object Test {
  case class Problem(w: Int, s: Long)

  trait Solver {
    def solve(p: Problem): Double
  }

  case class SolverA(par: Int) extends Solver {
    override def solve(p: Problem): Double = p.w / par.toDouble
  }

  case class SolverB(par: Double) extends Solver {
    override def solve(p: Problem): Double = math.log(p.w * p.s) / par
  }

  def main(args: Array[String]) {
    val width: ValuedNode[Int] = IntP("width", Fixed(2))
    val seed: ValuedNode[Long] = Seed("seed.problem")
    val problem = TypedComputation("problem", (width,seed))(
      (w: Int, s: Long) => {
        var i = 0
        var r = 0
        while(i < 1000 * w){
          var j = 0
          while(j < 1000000){
            r += j
            j += 1
          }
          i += 1
        }
        Problem(r,s)
      }
    ).report("result", (p: Problem) => p.w.toString)

    SeededGraph.run(Set(problem), args)
  }
}
