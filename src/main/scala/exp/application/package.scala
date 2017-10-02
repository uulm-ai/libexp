package exp

import java.io.PrintStream
import java.util.Date

import cats.Cartesian
import exp.cli._
import exp.computation.{CGraph, SimpleParallelEvaluator}
import exp.node._
import exp.node.syntax.N

/**
  * Created by thomas on 19.01.16.
  */
package object application {
  def runStandaloneExperiment(n: N[Any], desc: String, args: Array[String], out: PrintStream = System.out, commentSymbol: Option[Char] = Some('#')): Unit = {
    val cli: CLI[CGraph] = createParser(n)

    val parallelismOpt: CLI[Int] = CliOpt[Int](
      "parallelism",
      Read.fromParser(parsers.pPosInt),
      "the maximum number of computations started simultaneously",
      None,
      Some(4),
      "positive integer")

    val cliWithOpts: CLI[(CGraph,Int)] = Cartesian.tuple2(cli,parallelismOpt)

    commentSymbol.foreach{comSym =>
      out.println(s"$comSym produced by: $desc")
      out.println(s"$comSym date: ${new Date}")
      out.println(s"$comSym program arguments: ${args.mkString(" ")}")
    }
    //check for help
    if(args.toSet == Set("--help")){
      System.err.println(helpText(cliWithOpts))
    } else {
      runCliFree(args,cliWithOpts).map{ case (cg, parallelism) =>
        (cg.reports,SimpleParallelEvaluator.evalStream(cg, parallelism))
      }.fold(
        //treat error
        es => System.err.println("error while parsing command line:\n\t- " + es),
        //run experiment
        {
          case (reports,vals) =>
            val sortedReps = reports.sortBy(_.name)
            out.println(sortedReps.map(_.name).mkString("\t"))
            vals
              .map(v => sortedReps.map(c => c.f(v(c.node))).mkString("\t"))
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
