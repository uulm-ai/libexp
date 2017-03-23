package exp

import java.io.PrintStream

import exp.cli._
import exp.computation.{CGraph, SimpleParallelEvaluator}
import exp.node.syntax.N
import exp.node._
import cats.Cartesian
import cats.syntax.cartesian._

/**
  * Created by thomas on 19.01.16.
  */
package object application {
  def runStandaloneExperiment(n: N[Any], desc: String, args: Array[String], out: PrintStream = System.out): Unit = {
    val cli: CLI[CGraph] = createParser(n)

    val parallelismOpt: CLI[Int] = CliOpt[Int](
      "desired-parallelism",
      Read.fromParser(parsers.pPosInt),
      "the maximum number of computations started simultaneously",
      None,
      Some(Runtime.getRuntime.availableProcessors * 2),
      "positive integer")

    val cliWithOpts: CLI[(CGraph,Int)] = Cartesian.tuple2(cli,parallelismOpt)

    //check for help
    if(args.toSet == Set("--help")){
      System.err.println(helpText(cliWithOpts))
    } else {
      runCliFree(args,cliWithOpts).map{ case (cg, parallelism) =>
        (cg.reports,SimpleParallelEvaluator.evalStream(cg, parallelism))
      }.fold(
        es => System.err.println("encountered error during parse:\n\t- " + es),
        {
          case (reports,vals) =>
            out.println(reports.map(_.name).mkString("\t"))
            vals
              .map(v => reports.map(c => c.f(v(c.node))).mkString("\t"))
              .foreach(out.println)
        }
      )
    }
  }
}
