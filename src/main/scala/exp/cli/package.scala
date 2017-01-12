package exp

import com.typesafe.scalalogging.StrictLogging
import exp.cli.CliOpt._
import exp.computation.SimpleParallelEvaluator
import exp.node.{Node, _}
import fastparse.all._
import fastparse.core.Result

import scala.language.implicitConversions
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import scalaz.{Node => _, _}

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

  def runWithHelp[T](cf: CLI[T])(args: Array[String]): T = {
    //check for help
    if(args.toSet == Set("--help")){
      println(helpText(cf))
      sys.exit(0)
    } else {
      runCliFree(args,cf).fold(
        {
          e =>
            println("error parsing arguments: " + Some(args).filterNot(_.isEmpty).map(_.mkString(" ")).getOrElse("<no arguments given>"))
            println(e.list.mkString("\n"))
            println("\nusage:")
            println(helpText(cf))
            sys.exit(-1)
        },
        identity
      )
    }
  }

  def helpText(cf: CLI[_]): String = extractOptions(cf).map(_.helpText).mkString("\n")

  case class Read[+T](f: String => Val[T]) extends (String => Val[T]) {
    def filter(p: T => Boolean, msg: T => String = "invalid argument: " + _): Read[T] = Read(in =>
      f(in).flatMap(t => if(p(t)) msg(t).failureNel else t.successNel)
    )
    def map[B](g: T => B): Read[B] = Read(in => f(in).map(g))

    override def apply(v1: String): Val[T] = f(v1)
  }

  object Read {
    def getString: Read[String] = Read(_.successNel)
    def fromParser[T](p: P[T]): Read[T] = Read((s: String) => P(p ~ End).parse(s) match {
      case f@Result.Failure(x,y) =>
        logger.debug("failed parse: " + f, f)
        s"failed to parse input: $f".failureNel
      case s@Result.Success(value,_) =>
        logger.debug(s"successful parse: " + s, s)
        value.successNel
    })
  }

  @deprecated("use Read.fromParser")
  def parserToReader[T](p: P[T]): Read[T] = Read((s: String) => P(p ~ End).parse(s) match {
    case f@Result.Failure(x,y) =>
      logger.debug("failed parse: " + f, f)
      s"failed to parse input: $f".failureNel
    case s@Result.Success(value,_) =>
      logger.debug(s"successful parse: " + s, s)
      value.successNel
  })

  val seedOpt: CLI[Seq[Long]] = CliOpt[Seq[Long]](
    "seeds",
    Read.fromParser(P(parsers.pPosLong ~ ":" ~ parsers.pPosLong).map(x => x._1 to x._2)),
    "the set of RNG seeds to use",
    Some('s'),
    Some(Seq(1)),
    "n:m for the range `n` to `m`")

  @deprecated("use exp.application instead")
  def runStandaloneExperiment[S <: Stage](n: Node[S,_], desc: String, args: Array[String])(implicit ev: StageCast[S,CliProc.type]): Unit = {
    val cli: CLI[Node[RngInsertion.type, Any]] = CliProc.createCLI(ev(n))

    val cliWithSeed: CLI[(Node[RngInsertion.type, Any], Seq[Long])] = ^(cli,seedOpt)((_,_))

    //check for help
    if(args.toSet == Set("--help")){
      println(helpText(cliWithSeed))
    } else {
      val result = runCliFree(args,cliWithSeed).map{ case (node, seeds) =>
        val cg = Base.toCGraph(RngInsertion.insertSeeds(seeds, node))
        (cg.reports,SimpleParallelEvaluator.evalStream(cg))
      }

      result.fold(
        es => println("encountered error during parse:\n\t- " + es),
        {
          case (reports,vals) =>
            println(reports.map(_.name).mkString("\t"))
            println(vals.map(v => reports.map(c => c.f(v(c.node))).mkString("\t")).mkString("\n"))
        }
      )
    }
  }
}