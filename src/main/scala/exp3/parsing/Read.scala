package exp3.parsing

/** Type-class to represent types that can be parsed from a String representation. */
trait Read[A] {
  def parser: fastparse.P[A]
}
