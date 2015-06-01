/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3.parsing

import exp3.{Fixed, Stratification, StratParser}
import org.parboiled2.{Rule1, ParserInput}
import org.specs2.Specification
import org.specs2.specification.Fragments

/**
 * Created by thomas on 01.06.15.
 */
class ParserTest extends Specification {
  def intStratParser(s: ParserInput): StratParser[Int] = new StratParser[Int] {
    /** Parser that parses a single value. */
  override def singleValue: Rule1[Int] = Int

    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = ""

    override def input: ParserInput = s
  }
  def doubleStratParser(s: ParserInput): StratParser[Double] = new StratParser[Double] {
    /** Parser that parses a single value. */
  override def singleValue: Rule1[Double] = Float

    /** Describe syntax of specifying single values. */
    override def singleValueFormat: String = ""

    override def input: ParserInput = s
  }

  override def is: Fragments =
    "parse single int" ! {
      val p = intStratParser("5").nd.run()
      p.isSuccess and (p.get === Fixed(5))
    } ^
      "parse single float" ! {
      val p = doubleStratParser("5.0").nd.run()
      p.isSuccess and (p.get === Fixed(5.0))
    }
}
