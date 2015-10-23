package exp3.parsing

import fastparse.all._

/** Type-class to represent types that can be parsed from a String representation. */
trait Read[A] {
  def parser: P[A]
}
