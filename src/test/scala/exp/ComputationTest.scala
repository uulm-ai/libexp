package exp

import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mutable.Specification

import scalaz.Validation

/**
  * Created by thomas on 18.11.15.
  */
class ComputationTest extends Specification {

  "foo" >> (1 === 1)
  "simple lift test" >> {
    val lifted: LiftND[Int] = Fixed("fixed", IndexedSeq(Stream(1,2),Stream(3,4))).lift(2)
    val result = for{
      closed <- OpenQuery(Seq(lifted)).close(Map())
      str = Driver.evalGraph(closed,0L)
    } yield str
    result must beSuccessfulWith(haveLength(3))
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
