package exp.predefs

import exp.cli._
import exp.node.syntax._
import exp.parsers._
import fastparse.all
import fastparse.all._

trait PredefSyntax  {

  val intRangeParser: P[Seq[Int]] = P(
    (pPosInt ~ ":" ~ pPosInt).map(se => se._1 to se._2)
      | ("{" ~ pPosInt.rep(min = 1, sep = ",") ~ "}")
      | pPosInt.map(Seq(_)))

  def intInput(name: String,
               description: String,
               default: Option[Seq[Int]] = None): N[Int] =
    cliSeq(
      name,
      description = description,
      parser = Read.fromParser(intRangeParser),
      format = "integer: either plain '4', list '{1,3,6}', or inclusive range 'start:stop'",
      default = default
    ).reportAs(name)

  val doubleRangeParser: P[Seq[Double]] = P(
    (pDouble ~ ":" ~ pDouble ~ ":" ~ pPosInt).map{case (f,t,steps) => Seq.iterate(f,steps)(_ + ((t-f)/(steps - 1)))}
      | ("{" ~ pDouble.rep(min = 1, sep = ",") ~ "}")
      | pDouble.map(Seq(_))
  )

  def doubleInput(name: String,
                  description: String,
                  default: Option[Seq[Double]] = None): N[Double] =
    cliSeq(
      name = name,
      parser = Read.fromParser(doubleRangeParser),
      format = "double: either plain '1e-3', list '{1.0,3,4e2}', or exclusive range '1.0:10:4' (1 to 10 in 4 steps)",
      description = description,
      default = default)
      .reportAs(name)

  def stringEnum(name: String, values: Seq[String], description: String, defaults: Seq[String] = Seq()): N[String] =
    cliSeq[String](
      name = name,
      parser = Read.fromParser(StringIn(values:_*).!.map(Seq(_)) | P("{" ~ StringIn(values:_*).!.rep(min = 1, sep = ",") ~ "}")),
      format = s"one of the strings (${values.mkString(",")}), or a ',' separated list of them incurly braces: {foo,bar}",
      description = description,
      default = Some(defaults).filterNot(_.isEmpty)
    )
      .reportAs(name)

  /** Create an enumeration parameter by supplying some objects and a `toString` method. */
  def enum[T](name: String, description: String, values: Seq[T], toString: T => String = (_: T).toString, defaults: Seq[T]): N[T] =
    stringEnum(name, values.map(toString), description, defaults.map(toString))
      .map(values.map(v => toString(v) -> v).toMap, s"$name.map.from.cli-string")
}
