package exp

import exp.cli.CliOpt
import exp.node.syntax._
import exp.parsers._
import fastparse.all._

/**
  * Created by thomas on 15.12.15.
  */
package object predefs {

  val intRangeParser = P(
    (pInt ~ ":" ~ pInt).map(se => se._1 to se._2)
      | ("{" ~ pInt.rep(min = 1, sep = ",") ~ "}")
      | pInt.map(Seq(_)))

  def intInput(name: String,
               description: String,
               default: Option[Seq[Int]] = None): CliNode[Int] =
    fromCli(CliOpt(
      name,
      exp.cli.parserToReader(intRangeParser.map(xs => fromSeq(xs, name))),
      formatDescription = "integer: either plain '4', list '{1,3,6}', or inclusive range 'start:stop'",
      description = description,
      default = default.map(fromSeq(_,name)))
    ).addColumn(name,_.toString)

  val doubleRangeParser = P(
    (pDouble ~ ":" ~ pDouble ~ ":" ~ pInt).map{case (f,t,steps) => Seq.iterate(f,steps)(_ + ((t-f)/(steps - 1)))}
      | ("{" ~ pDouble.rep(min = 1, sep = ",") ~ "}")
      | pDouble.map(Seq(_))
  )

  def doubleInput(name: String,
                  description: String,
                  default: Option[Seq[Double]] = None): CliNode[Double] =
    fromCli(CliOpt(
      name,
      exp.cli.parserToReader(doubleRangeParser.map(xs => fromSeq(xs, name))),
      formatDescription = "double: either plain '1e-3', list '{1.0,3,4e2}', or exclusive range '1.0:10:4' (1 to 10 in 4 steps)",
      description = description,
      default = default.map(fromSeq(_,name)))).addColumn(name,_.toString)
}
