package exp

import org.specs2.matcher.{MustExpectable, Expectable, MatchResult, Matcher}

import scalaz.Validation

/**
  * Created by thomas on 18.11.15.
  */
trait ValMatchers {
  def beSuccessfulWith[T](m: Matcher[T]): Matcher[Validation[_,T]] =
    new Matcher[Validation[_, T]] {
      def apply[S <: Validation[_, T]](value: Expectable[S]): MatchResult[S] = {
        value.value.fold(
          x => Matcher.failure("Validation failed: " + x.toString, value),
          { (t: T) =>
            val result = m.apply(MustExpectable(t))
            if(result.isSuccess)
              Matcher.success("ok",value)
            else
              Matcher.failure("Validation is successful, but " + result.message, value)
          }
        )
      }
    }

  def beFailure: Matcher[Validation[_,_]] = new Matcher[Validation[_,_]] {
    override def apply[S <: Validation[_, _]](t: Expectable[S]): MatchResult[S] = {
      t.value.fold(
        x => Matcher.success("Success: Validation is failure: " + x, t),
        x => Matcher.failure("Failure: Validation is success: " + x, t)
      )
    }
  }

  def beSuccess: Matcher[Validation[_,_]] = new Matcher[Validation[_,_]] {
    override def apply[S <: Validation[_, _]](t: Expectable[S]): MatchResult[S] = {
      t.value.fold(
        x => Matcher.failure("Validation is failure: " + x, t),
        x => Matcher.success("Validation is success: " + x, t)
      )
    }
  }
}
