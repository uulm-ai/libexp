package exp

import exp.Computation.CompTyper
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mutable.Specification
import shapeless._

import scalaz.Validation

/**
  * Created by thomas on 18.11.15.
  */
class ComputationTest extends Specification {

  def evalClosed(nodes: Node[_]*): Val[Stream[Valuation]] = for{
    closed <- OpenQuery(nodes).close(Map())
    str = Driver.evalGraph(closed,0L)
  } yield str

  "simplest test" >>
    (evalClosed(Fixed("f",1 to 5)) must beSuccessfulWith(haveLength(5)))
  "simple lift test" >>
    (evalClosed(Fixed("fixed", IndexedSeq(Stream(1,2),Stream(3,4))).lift(2)) must beSuccessfulWith(haveLength(4)))
  "cross product 2x2" >> {
    val i1: Node[Int] = Fixed("i1", 1 to 3)
    val i2: Node[Int] = Fixed("i2", 1 to 2)
    val c = Computation("paste")((i1,i2))((i1: Int, i2: Int) => i1 + i2)
    evalClosed(c) must beSuccessfulWith(haveLength(6))
  }


  def beSuccessfulWith[T](m: Matcher[T]): Matcher[Validation[_,T]] =
    new Matcher[Validation[_, T]] {
      def apply[S <: Validation[_, T]](value: Expectable[S]): MatchResult[S] = {
        value.value.fold(
          x => Matcher.failure("Validation is failure, but must be success", value),
          { (t: T) =>
            val result = m.apply(t.as(identity))
            if(result.isSuccess)
              Matcher.success("ok",value)
            else
              Matcher.failure("Validation is successful, but " + result.message, value)
          }
        )
      }
    }
}
