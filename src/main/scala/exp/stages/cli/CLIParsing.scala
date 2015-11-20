package exp.stages.cli

import exp._
import exp.stages.{Stage, RNGInsertion}

trait CLIParsing extends Stage {
  type N[+A] <: Node[A] {type S <: CLIParsing}
}
/**
  * Created by thomas on 20.11.15.
  */
object CLIParsing extends CLIParsing {
//  type I = Array[String]
//  type Out = Context.WithAny[RNGInsertion.N]
//
//  def runStage(cliArgs: Array[String]): Val[(Context.WithAny[N]) => Out] = ???

  /** An input node without dependencies.
    * This node cannot be evaluated, it gets substituted with a different node at pre-processing time. */
  case class FromCLI[+T](cliOpt: CliOpt[RNGInsertion.N[T]]) extends Node[T] {
    type S = CLIParsing.type
  }

  //
  //object FromCLI extends StrictLogging {
  //  def ndParser[T](p: P[T]): P[Seq[T]] = (p.map(Seq(_)) | P("{" ~ p.rep(sep=P(",")) ~ "}")) ~ End
  //
  //  def singleValue[T](name: String, p: String => Val[T], default: Option[T], desc: String, short: Option[Char] = None): FromCLI[T] =
  //    FromCLI(name, p andThen (_.map(Fixed.single(name, _))), default.map(Fixed.single(name,_)), desc, short)
  //
  //  def multipleFromParser[T](name: String,
  //                            p: P[T],
  //                            default: Option[Seq[T]] = None,
  //                            desc: String = "",
  //                            short: Option[Char] = None): FromCLI[T] =
  //    FromCLI(
  //      name,
  //      CliOpt.parserToReader(ndParser(p).map(s =>  Fixed(name,s.toIndexedSeq))),
  //      default.map(s => Fixed(name,s.toIndexedSeq)),
  //      desc,
  //      short)
  //}
}
