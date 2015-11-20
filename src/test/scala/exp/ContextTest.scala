package exp

import exp.stages.evaluation.{Evaluation, Computed}
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import scalaz.syntax.apply._

class ContextTest extends Specification {

  "cnversion of fixed to context" >> {
    val n: Computed[Evaluation, Int] = fromSeq(1 to 5)
    n.withName("foo") must haveAnnotationFor(n, Name("foo"))
  }

//  "apply must keep reports for both inputs" >> {
//    val f1: Context[Int,Evaluation#N] = fromSeq(1 to 2).withName("f1")
//    val f2: Context[Int,Evaluation#N] = fromSeq(1 to 2).withName("f2")
//
//    val r: Context[Int,Evaluation#N] = ^(f1,f2)(_ + _)(Context.applyInstance).withName("r")
//
//    (r must haveAnnotationFor(f1, Name("f1"))) and
//      (r must haveAnnotationFor(f1, Name("f2"))) and
//      (r must haveAnnotationFor(r, Name("r")))
//  }

  def haveAnnotationFor[N[+_],T](n: N[T], a: Annotation): Matcher[Context[T,N]] =
    beSome(contain(a)) ^^ ((_:Context[T,N]).annotations.get(n))
}
