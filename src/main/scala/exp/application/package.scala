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

//  def saveGraph(n: Node[_], args: Array[String], fileName: String): Unit = {
//    import vultura.util.graph.graphviz._
//    import exp.node._
//
//    val parser: CLI[(Seq[Long]) => CGraph] = createParser(n)
//
//    val argsWithoutSeed = {
//      val (pre,post) = args.span(s => s != "--seed" && s != "-s")
//      pre ++ post.drop(2)
//    }
//    exp.cli
//      .runCliFree(argsWithoutSeed, parser)
//      .fold(
//        err => logger.warn("error rendering computation graph: " + err.toString),
//        {
//          runCG =>
//            val cg = runCG(Seq(0))
//            logger.info(s"rendering computation graph to file $fileName.pdf")
//            val dot: DotGraph[CNode, (CNode, CNode)] = DotGraph(
//              cg.nodeClosure,
//              cg.nodeClosure.flatMap(n => n.ins.map(_ -> n))
//            )
//
//            dot
//              .labelNodes{ case x => x.name}
//              .addNodeAttribute{
//                case _: CedgeND => Color.ORANGE
//              }
//              .renderPDF(fileName)}
//      )
//  }
}
