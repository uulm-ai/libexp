package exp.parser

import org.specs2._
import org.specs2.specification.Fragments
import exp.Exp
import shapeless._

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 8/26/13
 */
class package$Test extends Specification with matcher.ParserMatchers{

  object parsers extends ExpParser{
    def pSum = parsers.intExp ~ (literal("+") ~ parsers.intExp)
    def pSimple = literal("+") ~ parsers.intExp
    def pSumPrecedence = parsers.intExp ~ literal("+") ~ parsers.intExp
  }

  def is: Fragments =
    "parse int range" !
      (parsers.intExp must succeedOn("[1:2]").withResult(Exp.values(1,2))) ^
    "parse string literal" !
      (parsers.stringExp must succeedOn("navigation").withResult(Exp.values("navigation"))) ^
    "parse sequence of string" !
      (parsers.stringExp must succeedOn("{nav,foo}").withResult(Exp.values("nav","foo"))) ^
    "correct type of lift for simplest" !
      (parsers.intExp.lift must beAnInstanceOf[parsers.Parser[Exp[Int :: HNil]]]) ^
    "correct type of lift for simple" !
      (parsers.pSimple.lift must beAnInstanceOf[parsers.Parser[Exp[String :: Int :: HNil]]]) ^
    "correct type of lift" !
      (parsers.pSum.lift must beAnInstanceOf[parsers.Parser[Exp[Int :: String :: Int :: HNil]]]) ^
    "correct type of liftExp" !
      (parsers.pSum.lift.liftExp must beAnInstanceOf[PExpT[parsers.type,Int :: String :: Int :: HNil]]) ^
    "parse with lifted parser" !
      (parsers.pSum.lift must succeedOn("1 + 2").withResult(Exp.values(1 :: "+" :: 2 :: HNil))) ^
    "parse with expT parser" !
      (parsers.pSum.lift must succeedOn("[1:2] + 2").withResult(Exp.values(1 :: "+" :: 2 :: HNil, 2 :: "+" :: 2 :: HNil))) ^
    "parse with expT parser, without overriding precedence of `~`" !
      (parsers.pSumPrecedence.lift must succeedOn("[1:2] + 2").withResult(Exp.values(1 :: "+" :: 2 :: HNil, 2 :: "+" :: 2 :: HNil))) ^
    "map expT and use it to parse something" !
      ((for(x1 :: _ :: x2 :: HNil <- (parsers.intExp ~ "+" ~ parsers.intExp).llE) yield (x1 + x2)).run must succeedOn("[1:2] + 2").withResult(Exp.values(3,4))) ^
    "target syntax" !
      ((for(x1 ~ _ ~ x2 <- (parsers.intExp ~ "+" ~ parsers.intExp).llE) yield x1 + x2).run must succeedOn("[1:2] + [2:3]").withResult(Exp.values(3,4,4,5)))
}
