package exp.cli

import com.typesafe.scalalogging.StrictLogging
import exp._
import fastparse.all._
import fastparse.core.Result

import scalaz._
import scalaz.std.list._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._

/** Specifies how to obtain a value of type `T` from command-line arguments.
  *
  * @param long String for long argument name, like '--foo'.
  * @param short Optional character for short argument name, like '-f'.
  * @param valueParser Method to convert the string after the argument to a value of type `T`.
  * @param default If no default value is specified, the argument is required.
  * @param description Description of the purpose of this option.
  */
case class CliOpt[+T](long: String,
                      valueParser: Read[T],
                      description: String = "no description given",
                      short: Option[Char] = None,
                      default: Option[T] = None,
                      formatDescription: String = "format unknown"){
  def argIdentifiers: Seq[String] = Seq(s"--$long") ++ short.map(s => s"-$s")
  def helpText: String =
    s"""--$long${short.map(c => s" | -$c").getOrElse("")}
       |    $description
       |    format: $formatDescription
       |    ${default.map(d => s"default: $d").getOrElse("required argument")}""".stripMargin
}

case class CliOptList[+R](opts: Seq[CliOpt[R]]) {
  def apply(args: Array[String]): Val[Seq[R]] = CliOpt.parse(args,opts)
  def helpText: String = opts.map(_.helpText).mkString("\n")
}

object CliOpt extends StrictLogging {
  def validateDistinct[T](ts: Seq[T]): Val[Seq[T]] = ts.successNel[String]
      .ensureNel("found duplicate argument during construction of CLI", xs => xs.distinct.size == xs.size)

  def parse[LUB](args: Array[String], opts: Seq[CliOpt[LUB]]): Val[Seq[LUB]] = {
    def parsePair(key: String, data: String): Val[(CliOpt[LUB],LUB)] = for{
      opt <- opts.find(_.argIdentifiers.contains(key)).toSuccessNel(s"there is no option named $args")
      value <- opt.valueParser(data).leftMap(errs => s"cannot parse argument '--${opt.long}' with input '$data'" <:: errs)
    } yield opt -> value

    for {
      _ <- validateDistinct(opts.flatMap(_.argIdentifiers))
      kv <- args.grouped(2).toList
        .successNel[String].ensureNel("requiring an even number of arguments", _.forall(_.length % 2 == 0))
      parsedKV <- kv.map(two => parsePair(two(0),two(1))).sequence
      parsedResult = parsedKV.toMap
      result <- opts.toList.map(co => parsedResult.get(co).orElse(co.default).toSuccessNel(s"missing required argument  '--${co.long}'")).sequenceU
    } yield result
  }
}