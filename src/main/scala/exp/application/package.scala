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
      Read.fromParser(P(parsers.pPosLong ~ ":" ~ parsers.pPosLong).map(x => x._1 to x._2)),
      "the set of RNG seeds to use",
      Some('s'),
      Some(Seq(1)),
      "n:m for the range `n` to `m`")

    val parallelismOpt: CLI[Int] = CliOpt[Int](
      "desired-parallelism",
      Read.fromParser(parsers.pPosInt),
      "the maximum number of computations started simultaneously",
      None,
      Some(Runtime.getRuntime.availableProcessors * 2),
      "positive integer")

    val cliWithSeed: CLI[(Seq[Long] => CGraph,Seq[Long],Int)] = ^^(cli,seedOpt,parallelismOpt)((_,_,_))

    //check for help
    if(args.toSet == Set("--help")){
      println(helpText(cliWithSeed))
    } else {
      runCliFree(args,cliWithSeed).map{ case (node, seeds, parallelism) =>
        val cg = node(seeds)
        (cg.reports,SimpleParallelEvaluator.evalStream(cg, parallelism))
      }.fold(
        es => println("encountered error during parse:\n\t- " + es),
        {
          case (reports,vals) =>
            println(reports.map(_.name).mkString("\t"))
            vals
              .map(v => reports.map(c => c.f(v(c.node))).mkString("\t"))
              .foreach(println(_))
        }
      )
    }
  }
}
