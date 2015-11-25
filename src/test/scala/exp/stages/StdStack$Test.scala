package exp.stages

import org.specs2.mutable.Specification

import scala.util.Random

/**
  * Created by thomas on 23.11.15.
  */
class StdStack$Test extends Specification {

  "testing std stack syntax" >> {
    import StdStack.Ops._
    import scalaz.syntax.apply._

    val prod = ^(
      fromSeq(1 to 5).withName("x1"): StdStack.N[Int],
      fromSeq(1 to 5).withName("x2"): StdStack.N[Int]
    )(_ * _)
    val gaussian = getSeed("test").map(new Random(_).nextGaussian())
    val prodPlusRand = ^(gaussian,prod)(_ + _).withName("result")

    1 === 2
  }

}
