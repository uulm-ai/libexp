package exp

import fastparse.all._
import fastparse.core.Result._

import scalaz.{Validation => ZValidation}
import scalaz.syntax.validation._

/**
  * Created by thomas on 18.11.15.
  */
case class CliOpt[+T](long: String, short: Option[Char], valueParser: P[T], default: Option[T], description: String){
  def argNameP: P[Unit] = P(("--" ~ long) | ("-" ~ short.map(c => CharIn(Seq(c))).getOrElse(P(Fail))))
  def cliParser(sep: P[Unit]): P[T] = argNameP ~ sep ~ valueParser
}


object CliOpt {
  val sepChar = '\t'
  val sepP: P[Unit] = P(CharIn(Seq(sepChar)))

  def parse[LUB](args: Array[String], opts: Seq[CliOpt[LUB]]): Val[Seq[LUB]] = {
    val p: P[Seq[LUB]] = opts.map(opt => opt.cliParser(sepP)).reduce(_ | _).rep(sep = sepP)

    p.parse(args.mkString(sepChar.toString)) match {
      case Failure(x,y) => s"failed to parse input: $x".failureNel
      case Success(value,_) => value.successNel
    }
  }
}