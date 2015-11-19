import scalaz.{NonEmptyList, ValidationNel}
import scalaz.syntax.validation._

/**
  * Created by thomas on 18.11.15.
  */
package object exp {
  type Val[+T] = ValidationNel[String,T]

  implicit class RichT[T](val t: Val[T]) {
    def ensureNel(err: String, test: T => Boolean): Val[T] = t.ensure(NonEmptyList(err))(test)
  }
}
