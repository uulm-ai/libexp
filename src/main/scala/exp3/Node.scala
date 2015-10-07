/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import fastparse.P
import scopt.{OptionParser, Read}

import scala.language.reflectiveCalls

/** A node in the computation graph.
  * During one configuration, it holds a value of type `T`.
  * It produces some output columns in generated table. */
sealed trait Node extends Serializable {
  /** The name of this node. */
  def name: String

  override def hashCode(): Int = name.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case n: Node => n.name == name
    case _ => false
  }
}

sealed trait ValuedNode[T] extends Node {
  /** The columns produced by this node. */
  def columns: Seq[(String,T => String)] = Seq()
}

trait NDParser[T] {
  def syntaxDescription: String
  def nd: P[NonDeterminism[T]]
}

trait UntypedComputation[T] extends ValuedNode[T]{
  def predecessors: Seq[Node]
  def compute(args: Seq[Any]): T
}

case class UTC[T](name: String, computation: Seq[Any] => T, predecessors: Seq[Node], override val columns: Seq[(String, T => String)]) extends UntypedComputation[T] {
  override def compute(args: Seq[Any]): T = computation(args)
}


/** An InputNode has is a independent variable.
  * It has no dependencies and can be fed compatible values to trigger computations in the successor nodes. */
trait InputNode[T] extends ValuedNode[T] {
  def default: NonDeterminism[T]
  def parser: NDParser[T]

  /** Replace spaces and dots with '-'. */
  def toOptionName(s: String): String = s.map{
    case ' ' => '-'
    case '.' => '-'
    case other => other
  }

  /** Installs a handler for the current InputNode within a scopt CLI-Parser. */
  def install(optParser: OptionParser[(RunConfig,Map[InputNode[_], NonDeterminism[_]])]): Unit = {
    import fastparse._
    implicit val tReader: Read[NonDeterminism[T]] = Read.reads(s => parser.nd.parse(s) match {
      case Result.Success(x,_) => x
      case o                 => sys.error(o.toString)
    })
    optParser
      .opt[NonDeterminism[T]](toOptionName(name))
      .action{case (si,(rc,m)) => (rc,m + (this -> si))}
      .text(s"$name; default is $default")
  }

  /** The columns produced by this node. */
  override val columns: Seq[(String, T => String)] = Seq(name -> (_.toString))
}