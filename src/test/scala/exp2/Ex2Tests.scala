package exp2

import org.specs2._
import matcher._
import org.specs2.specification.Fragments

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 8/29/13
 */
class Ex2Tests extends Specification with Ex2Matchers {

  def is: Fragments =
   "simple cross-product" ! ((Ex2.values(1,2) cross Ex2.values(10,11)) must haveEqualTrialsAs(Ex2.values((1,10),(1,11),(2,10),(2,11)))) ^
   "mapping an experiment should return correct result" ! {
     val mapped = (for(i <- Ex2.values(1,2).logAs("i")) yield i + 2).logAs("m")
     mapped.run.toList === List(Seq("1","3"),Seq("2","4")) and mapped.colNames === List("i","m")
   } ^
   "the mapping function must be applied only once for each trial" ! {
     var cnt = 0
     val f: Int => Int = { x =>
       cnt = cnt + 1
       x + 1
     }
     val mapped = Ex2.values(1,2,3).map(f).logAs("i").logAs("j")
     mapped.run.toList === List(Seq("2","2"), Seq("3","3"), Seq("4","4")) and cnt === 3
   }
//  ^
//   "larger tests" ! {
//     val exp = for{
//       i <- Ex2.values(1,2) logAs "i"
//       j <- Ex2.values(10,11) logAs "j"
//     } yield (i to j)
//     exp.run.toList === "foo"
//   }
}
