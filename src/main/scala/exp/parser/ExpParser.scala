package exp.parser

import scala.util.parsing.combinator.JavaTokenParsers
import exp.Exp

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 8/27/13
 */
trait ExpParser extends JavaTokenParsers {
  def boolP: Parser[Boolean] = ("true" | "TRUE" | "T") ^^ (_ => true) | ("false" | "FALSE" | "F") ^^ (_ => false)
  def intP: Parser[Int] = wholeNumber ^^ (_.toInt)

  def boolExp: Parser[Exp[Boolean]] = expify(boolP)

  def intExp: Parser[Exp[Int]] = expify(intP) | intRange

  def doubleExp: Parser[Exp[Double]] = expify(floatingPointNumber ^^ (_.toDouble))

  def stringExp: Parser[Exp[String]] = expify(ident)

  def expify[T](p: Parser[T]): Parser[Exp[T]] = listedExp(p) | (p ^^ (Exp.values(_)))
  def listedExp[T](p: Parser[T]): Parser[Exp[T]] = "{" ~> (repsep(p,",") <~ "}") ^^ Exp.fromIterable | p ^^ (Exp.values(_))
  def intRange: Parser[Exp[Int]] = for(start ~ _ ~ end <- "[" ~> intP ~ ":" ~ intP <~ "]") yield Exp.fromIterable(start to end)
}
