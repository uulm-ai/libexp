/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp.parser

import org.specs2._
import org.specs2.specification.Fragments
import exp.Exp

class package$Test extends Specification with matcher.ParserMatchers{

  object parsers extends ExpParser {
    def pSum = parsers.intExp ~ (literal("+") ~ parsers.intExp)
    def pSimple: parsers.Parser[parsers.~[String, Exp[Int]]] = literal("+") ~ parsers.intExp
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
      (parsers.intExp.lift must beAnInstanceOf[PExpT[_,Int]]) ^
    "correct type of lift for simple" !
      (parsers.pSimple.lift must beAnInstanceOf[PExpT[parsers.type,(String,Int)]]) ^
    "correct type of lift" !
      (parsers.pSum.lift must beAnInstanceOf[PExpT[parsers.type,(Int,(String,Int))]]) ^
    "target syntax" !
      ((for(x1 ~~ _ ~~ x2 <- (parsers.intExp ~ "+" ~ parsers.intExp).lift) yield x1 + x2).run must succeedOn("[1:2] + [2:3]").withResult(Exp.values(3,4,4,5))) ^
    "target syntax" !
      ((for(x1 ~~ x2 <- ((parsers.intExp <~ "+") ~ parsers.intExp).lift) yield x1 + x2).run must succeedOn("[1:2] + [2:3]").withResult(Exp.values(3,4,4,5))) ^
     "target syntax, three parts" !
      ((for(x1 ~~ x2 ~~ x3 <- ((parsers.intExp <~ "+") ~ (parsers.intExp <~ "+") ~ parsers.intExp).lift) yield x1 + x2).run must succeedOn("[1:2] + [2:3] + [4:5]").withResult(Exp.values(7,8,8,9,8,9,9,10)))

}
