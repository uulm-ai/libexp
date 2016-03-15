package exp

import fastparse.all._

/**
  * Created by thomas on 27.11.15.
  */
package object parsers {
  val digits        = P(CharIn("1234567890").rep(min = 1))
  val exponent      = P(CharIn("eE") ~ CharIn("+-").? ~ digits )
  val fractional    = P("." ~ digits )
  val integral      = P("0" | CharIn('1' to '9') ~ digits.? )

  val pPosLong: P[Long] = digits.!.map(_.toLong)
  val pPosInt: P[Int] = digits.!.map(_.toInt)
  val pDouble: P[Double] = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(_.toDouble)
  val pBool: P[Boolean] = StringIn("true","TRUE","T","t","1").map(_ => true) | StringIn("false","FALSE","F","f","0").map( _ => false)
}
