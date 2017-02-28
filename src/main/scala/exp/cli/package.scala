package exp

import com.typesafe.scalalogging.StrictLogging
import exp.cli.CliOpt._
import fastparse.all._
import cats.free._
import cats.syntax.validated._
import cats.~>
import cats.instances.list._

import scala.language.{implicitConversions, reflectiveCalls}

/**
  * Created by thomas on 27.11.15.
  */
package object cli extends StrictLogging {

  type CLI[T] = FreeApplicative[CliOpt,T]

  implicit def liftCliOpt[T](co: CliOpt[T]): CLI[T] = FreeApplicative.lift(co)

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
      cf.foldMap(new ~>[CliOpt,cats.Id]{
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
            println(e.toList.mkString("\n"))
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
    def filter(p: T => Boolean, msg: T => String = "invalid argument: " + _): Read[T] = Read((in: String) =>
      f(in).fold(Left(_), t => if(p(t)) Left(msg(t)) else Right(t)): Val[T]
    )
    def map[B](g: T => B): Read[B] = Read(in => f(in).map(g))

    override def apply(v1: String): Val[T] = f(v1)
  }

  object Read {
    def getString: Read[String] = Read(Right(_))
    def fromParser[T](p: P[T]): Read[T] = Read((s: String) => P(p ~ End).parse(s) match {
      case f@Parsed.Failure(x,y,_) =>
        logger.debug("failed parse: " + f, f)
        Left(s"failed to parse input: $f")
      case s@Parsed.Success(value,_) =>
        logger.debug(s"successful parse: " + s, s)
        Right(value)
    })
  }

  @deprecated("use Read.fromParser")
  def parserToReader[T](p: P[T]): Read[T] = Read((s: String) => P(p ~ End).parse(s) match {
    case f@Parsed.Failure(x,y,_) =>
      logger.debug("failed parse: " + f, f)
      Left(s"failed to parse input: $f")
    case s@Parsed.Success(value,_) =>
      logger.debug(s"successful parse: " + s, s)
      Right(value)
  })

  val seedOpt: CLI[Seq[Long]] = CliOpt[Seq[Long]](
    "seeds",
    Read.fromParser(P(parsers.pPosLong ~ ":" ~ parsers.pPosLong).map(x => x._1 to x._2)),
    "the set of RNG seeds to use",
    Some('s'),
    Some(Seq(1)),
    "n:m for the range `n` to `m`")
}