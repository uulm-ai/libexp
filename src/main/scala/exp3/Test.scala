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
    val mode = EnumP("mode", Set("low","high"), only("low"))
    val width = IntP("width", only(2))
    val seed = Seed("seed.problem")
    val problem = TypedComputation("problem", (width,seed,mode))(
      (w: Int, s: Long, _: String) => {
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

    ExpApp("test").run(Set(problem), args)
  }
}
