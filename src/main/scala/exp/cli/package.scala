package exp

import com.typesafe.scalalogging.StrictLogging
import exp.cli.CliOpt._
import exp.computation.SimpleEvaluator
import exp.node.Node
import exp.node._
import fastparse.all._
import fastparse.core.Result


import scala.util.Random
import scalaz.{Node => _, _}
import std.list._
import scalaz.syntax.validation._
import scalaz.syntax.applicative._

/**
  * Created by thomas on 27.11.15.
  */
package object cli extends StrictLogging {

  type CLI[T] = FreeAp[CliOpt,T]

  implicit def liftCliOpt[T](co: CliOpt[T]): CLI[T] = FreeAp.lift(co)

  def extractOptions[T](cf: CLI[T]): List[CliOpt[_]] = {
    cf.analyze(new ~>[CliOpt, ({type L[X] = List[CliOpt[_]]})#L] {
      override def apply[A](fa: CliOpt[A]): List[CliOpt[_]] = List(fa)
    }).distinct.sortBy(_.long)
  }

  def runCliFree[T](args: Array[String], cf: CLI[T]): Val[T] = {
    val options: List[CliOpt[_]] = extractOptions(cf)

    for{
      parsed <- parse[Any](args, options)
      m = options.zip(parsed).toMap
    } yield {
      cf.foldMap(new ~>[CliOpt,Id.Id]{
        override def apply[A](fa: CliOpt[A]): A = m(fa).asInstanceOf[A]
      })
    }
  }

  def helpText(cf: CLI[_]): String = extractOptions(cf).map(_.helpText).mkString("\n")

  type Reader[+T] = String => Val[T]

  def parserToReader[T](p: P[T]): Reader[T] = (s: String) => P(p ~ End).parse(s) match {
    case f@Result.Failure(x,y) =>
      logger.debug("failed parse: " + f, f)
      s"failed to parse input: $f".failureNel
    case s@Result.Success(value,_) =>
      logger.debug(s"successful parse: " + s, s)
      value.successNel
  }

  val seedOpt: CLI[Seq[Long]] = CliOpt[Seq[Long]](
    "seeds",
    parserToReader(P(parsers.pLong ~ ":" ~ parsers.pLong).map(x => x._1 to x._2)),
    "the set of RNG seeds to use",
    Some('s'),
    Some(Seq(1)),
    "n:m for the range `n` to `m`")

  def runStandaloneExperiment[S <: Stage](n: Node[S,_], desc: String, args: Array[String])(implicit ev: StageCast[S,CliProc.type]): Unit = {
    val cli: CLI[Node[RngInsertion.type, Any]] = CliProc.createCLI(ev(n))

    val cliWithSeed: CLI[(Node[RngInsertion.type, Any], Seq[Long])] = ^(cli,seedOpt)((_,_))

    //check for help
    if(args.toSet == Set("--help")){
      println(helpText(cliWithSeed))
    } else {
      val result = runCliFree(args,cliWithSeed).map{ case (node, seeds) =>
        val baseNode = RngInsertion.insertSeeds(seeds,node)
        val cg = Base.toCGraph(baseNode)
        (cg,SimpleEvaluator.evalStream(cg))
      }

      result.fold(
        es => println("encountered error during parse:\n\t- " + es),
        {
          case (cg,vals) =>
            println(cg.reports.map(_.name).mkString("\t"))
            vals.foreach(v =>
              println(cg.reports.map(c => c.f(v(c.node))).mkString("\t"))
            )
        }
      )
    }
  }

  def runExperimentSuite(experiments: Seq[ExperimentMonadicCLI], args: Array[String]): Unit = ???
}

/** The type `Node[CliProc.type,_]` is monadic, as it is not possible to list the columns before evaluation of
  * cli arguments.
  *
  * @param node
  * @param id
  * @param description
  * @param version
  */
case class ExperimentMonadicCLI(node: Node[CliProc.type,_], id: String, description: String, version: String)

object Test {
  import exp.cli._
  def main(args: Array[String]) {
    import exp.node.syntax._
    import fastparse.all._

    val intOpt: CliOpt[BaseNode[Int]] = CliOpt(
      "test",
      parserToReader(
        exp.parsers.pInt
          .map(n => fromSeq(1 to n, "test.node"))
      ),
      "test input",
      default = Some(fromSeq(1 to 10, "test.node"))
    )

    val gaussian = seed("gaussian").map(new Random(_).nextGaussian())
      .addColumn("r")

    val n = ^(fromCli(intOpt),gaussian)(_ + _).addColumn("result")

    runStandaloneExperiment(n, "some nice experiment" , args)
  }
}
