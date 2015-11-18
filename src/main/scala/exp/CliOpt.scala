package exp

import com.typesafe.scalalogging.StrictLogging
import fastparse.all._
import fastparse.core.Result
import fastparse.core.Result._

import scalaz.NonEmptyList
import scalaz.syntax.validation._

import scalaz.syntax.traverse._
import scalaz.std.list._

/**
  * Created by thomas on 18.11.15.
  */
case class CliOpt[+T](long: String, short: Option[Char], valueParser: P[T], default: Option[T] = None, description: String = ""){
  def argNameP: P[Unit] = P(("--" ~ long) | ("-" ~ short.map(c => CharIn(Seq(c))).getOrElse(P(Fail))))
}


object CliOpt extends StrictLogging {
  val sepChar = '\t'
  val sepP: P[Unit] = P(CharIn(Seq(sepChar)))

  def toVal[T](pr: Result[T]): Val[T] = pr match {
    case f@Failure(x,y) =>
      logger.debug("failed parse: " + f, f)
      s"failed to parse input: $f".failureNel
    case s@Success(value,_) =>
      logger.debug(s"successful parse: " + s, s)
      value.successNel
  }

  def validateDistinct[T](ts: Seq[T]): Val[Seq[T]] = ts.successNel[String]
      .ensure(NonEmptyList("found duplicate argument during construction of CLI"))(xs => xs.distinct.size == xs.size)

  def parse[LUB](args: Array[String], opts: Seq[CliOpt[LUB]]): Val[Seq[LUB]] = {
    def argsForOpt(co: CliOpt[_]) = Seq("--" + co.long) ++ co.short.map("-" + _.toString)
    val starters: Seq[String] = opts.flatMap(argsForOpt)
    def optForArg(arg: String): CliOpt[LUB] = opts.find(o => argsForOpt(o).contains(arg)).get
    for{
      startParser <- validateDistinct(starters).map(ss => P(StringIn(ss:_*)))
      namedArgParser = P((startParser.! ~ sepP ~ CharsWhile(_ != sepChar).!).rep(sep = sepP) ~ End)
      parseInput: String = args.mkString(sepChar.toString)
      _ = logger.debug("parsing on input: " + parseInput)
      argTuples <- toVal(namedArgParser.parse(parseInput))
      _ = logger.debug(s"parsed argument tuples: $argTuples")
      nodes <- argTuples.map{case (arg,data) =>
          toVal(P(optForArg(arg).valueParser ~ End).parse(data))
      }.toList.sequence
    } yield nodes
  }
}