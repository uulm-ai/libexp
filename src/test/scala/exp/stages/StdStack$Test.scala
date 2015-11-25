package exp.stages

import exp.stages.StdStack.N
import exp.{Name, Context}
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification

import scala.util.Random

/**
  * Created by thomas on 23.11.15.
  */
class StdStack$Test extends Specification {

  "testing std stack syntax" >> {
    import StdStack.Ops._
    import scalaz.syntax.apply._

    val x1: N[Int] = fromSeq(1 to 5, "x1"): N[Int]
    val x2: N[Int] = fromSeq(1 to 5, "x2"): N[Int]
    val prod = ^(
      x1,
      x2
    )(_ * _)
    val gaussian: N[Double] = getSeed("test").map(new Random(_).nextGaussian())
    val prodPlusRand: N[Double] = ^(gaussian,prod)(_ + _).withName("result")

    prodPlusRand.annotations.foreach(println)
    (prodPlusRand must containNameFor(prodPlusRand.value, "result"))
  }

  def containNameFor[N[+_]](n: N[_], name: String): Matcher[Context[_,N]] =
    contain(beEqualTo(n -> Name(name))) ^^ ((_:Context[_,N]).annotations)

}
