package exp.stages.evaluation

import org.specs2.mutable.Specification
import scalaz.syntax.apply._
import exp._

class ComputedTest extends Specification {
  "apply test" >> {
    ^(fromSeq(1 to 5), fromSeq(2 to 3))(_ + _) must beTypedEqualTo(fromSeq(1 to 5))
  }
}
