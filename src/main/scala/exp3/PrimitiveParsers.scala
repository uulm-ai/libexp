package exp3

import fastparse.all._

object PrimitiveParsers {
  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
    def apply(t: T) = f(t)
    override def toString() = name

  }
  // Here is the parser
  val Whitespace = NamedFunction(" \n".contains(_: Char), "Whitespace")
  val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  val space         = P( CharsWhile(Whitespace).? )
  val digits        = P( CharsWhile(Digits))
  val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
  val fractional    = P( "." ~ digits )
  val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

  val integer: P[Int] = integral.!.map(_.toInt)
  val long: P[Long] = integral.!.map(_.toLong)
  val number: P[Double] = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(_.toDouble)

  val bool: P[Boolean] = P("true".!.map(_ => true) | "false".!.map(_ => false))
}
