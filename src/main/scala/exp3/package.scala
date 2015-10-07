/**
 * Created by thomas on 07.10.15.
 */
package object exp3 {

  def only[A](a: A): Stratification[A] = Stratification(Seq(a))

}
