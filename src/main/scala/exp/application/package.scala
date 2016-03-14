package exp

import exp.cli._
import exp.computation.{CGraph, SimpleParallelEvaluator}
import exp.node.applicative._
import exp.node.applicative.syntax.N
import fastparse.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
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
      runCliFree(args,cliWithSeed)
        .map{ case (node, seeds) =>
          val cg = node(seeds)
          (cg.reports,SimpleParallelEvaluator.evalStream(cg))}
        .fold(
        es => println("encountered error during parse:\n\t- " + es),
        {
          case (reports,vals) =>
            //print headers
            println(reports.map(_.name).mkString("\t"))
            //print rows
            vals
              .foreach(_.onSuccess({ case stream =>
                stream
                  .map(v => reports.map(c => c.f(v(c.node))).mkString("\t"))
                  .foreach(println(_))
              }))
            Await.ready(Future.sequence(vals), Duration.Inf) // prevents JVM from exiting
        }
      )
    }
  }
}
