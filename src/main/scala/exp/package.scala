import scalaz.ValidationNel

/**
  * Created by thomas on 18.11.15.
  */
package object exp {
  type Val[+T] = ValidationNel[String,T]
}
