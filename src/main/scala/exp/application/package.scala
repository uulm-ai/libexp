package exp

import exp.cli._
import exp.computation.{CGraph, SimpleParallelEvaluator}
import exp.node.applicative.syntax.N
import exp.node.applicative._
import fastparse.all._
import scalaz.syntax.applicative._

/**
  * Created by thomas on 19.01.16.
  */
package object application {
  def runStandaloneExperiment(n: N[Any], desc: String, args: Array[String]): Unit = {
    val cli: CLI[(Seq[Long]) => CGraph] = createParser(n)

    val seedOpt: CLI[Seq[Long]] = CliOpt[Seq[Long]](
      "seeds",
      Read.fromParser(P(parsers.pLong ~ ":" ~ parsers.pLong).map(x => x._1 to x._2)),
      "the set of RNG seeds to use",
      Some('s'),
      Some(Seq(1)),
      "n:m for the range `n` to `m`")

    val cliWithSeed: CLI[(Seq[Long] => CGraph,Seq[Long])] = ^(cli,seedOpt)((_,_))

    //check for help
    if(args.toSet == Set("--help")){
      println(helpText(cliWithSeed))
    } else {
      val result = runCliFree(args,cliWithSeed).map{ case (node, seeds) =>
        val cg = node(seeds)
        (cg.reports,SimpleParallelEvaluator.evalStream(cg))
      }

      result.fold(
        es => println("encountered error during parse:\n\t- " + es),
        {
          case (reports,vals) =>
            println(reports.map(_.name).mkString("\t"))
            println(vals.par.map(v => reports.map(c => c.f(v(c.node))).mkString("\t")).seq.mkString("\n"))
        }
      )
    }
  }
}
