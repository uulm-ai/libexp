package exp.node

sealed class =!=[A,B]

trait LowerPriorityImplicits {
  implicit def equal[A]: =!=[A, A] = sys.error("should not be called")
}
object =!= extends LowerPriorityImplicits {
  implicit def nequal[A,B](implicit same: A =:= B = null): =!=[A,B] =
    if (same != null) sys.error("should not be called explicitly with same type")
    else new =!=[A,B]
}
