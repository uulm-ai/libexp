libexp
======

A library for constructing computational experiments.

Example usage of the parser
---------------------------
You can lift a parser into the `PExpT` monad with `.lift`. 
This will automatically build a cross-product of the `Exp` monads (flatmaps them) within the parsers tuple type  (`~`). 
Extraction has to be performed with the double tilde `~~`, because of name clash with ordinary parser extractor
when used within a parser environment.

    object MyParser extends ExpParser{
        def grid: Parser[Exp[Problem]] = for (
            x ~~ y ~~ doms <- (("grid(" ~> intExp <~ ",") ~ (intExp <~ ",") ~ intExp <~ ")").lift
        ) yield generators.grid(x,y,doms)
    }

This parses for example `grid(1,{2,3,4},[2:6])`.

Usage
-----
To use the library, add the following lines to your `.sbt` file:

    resolvers += "tgeier repository" at "http://companion.informatik.uni-ulm.de/~tgeier/mvn"
    
    libraryDependencies += "de.uni-ulm" % "libexp_2.11" % "1.4.1"
