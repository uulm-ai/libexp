/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import org.parboiled2._
import scopt.{OptionParser, Read}
import shapeless.HList._
import shapeless.ops.hlist.{Comapped, ToTraversable}
import shapeless.ops.product.ToHList
import shapeless.ops.traversable.FromTraversable
import shapeless.syntax.std.traversable._
import shapeless.{HList, HNil}

import scala.language.reflectiveCalls
import scala.util.Try
import scala.{:: => _}


/** A node in the computation graph.
  * During one configuration, it holds a value of type `T`.
  * It produces some output columns in generated table. */
sealed trait Node {
  /** The name of this node. */
  def name: String
}

sealed trait ValuedNode[T] extends Node {
  /** The columns produced by this node. */
  def columns: Seq[(String,T => String)] = Seq()
}

trait NDParser[T] extends Parser {
  def syntaxDescription: String
  def nd: Rule1[NonDeterminism[T]]
  def Digits: Rule[HNil, HNil] = rule { oneOrMore(CharPredicate.Digit) }
}

/** An InputNode has is a independent variable.
  * It has no dependencies and can be fed compatible values to trigger computations in the successor nodes. */
trait InputNode[T] extends ValuedNode[T] {
  def default: NonDeterminism[T]
  def parser(s: ParserInput): NDParser[T]

  def parse(s: String): Try[NonDeterminism[T]] = {
    val p = parser(s)
    p.nd.run()
  }

  /** Replace spaces and dots with '-'. */
  def toOptionName(s: String): String = s.map{
    case ' ' => '-'
    case '.' => '-'
    case other => other
  }

  /** Installs a handler for the current InputNode within a scopt CLI-Parser. */
  def install(parser: OptionParser[Map[InputNode[_], NonDeterminism[_]]]): Unit = {
    implicit val tReader: Read[NonDeterminism[T]] = Read.reads(s => parse(s).get)
    parser
      .opt[NonDeterminism[T]](toOptionName(name))
      .action{case (si,m) => m + (this -> si)}
      .text(s"$name; default is $default")
  }

  /** The columns produced by this node. */
  override val columns: Seq[(String, T => String)] = Seq(name -> (_.toString))
}

/** A computed node depends on some other nodes to compute it's value.
  * The computation might require resources. */
sealed trait ComputedNode[DN <: HList,D <: HList,T] extends ValuedNode[T] {
  implicit def unwrap: Comapped.Aux[DN,ValuedNode,D]
  implicit def lub: ToTraversable.Aux[DN, List, Node]
  implicit def fromT: FromTraversable[D]
  def dependencies: DN
  def computation: D => T
  def predecessors: Seq[Node] = dependencies.toList[Node]
  def compute(args: Seq[Any]): T = computation(args.toHList[D].get)
}

case class Computation[DN <: HList, D <: HList, T](name: String, dependencies: DN)
                                                  (val computation: D => T)
                                                  (implicit
                                                   val unwrap: Comapped.Aux[DN,ValuedNode,D],
                                                   val lub: ToTraversable.Aux[DN, List, Node],
                                                   val fromT: FromTraversable[D]) extends ComputedNode[DN,D,T] { outer =>
  override def toString: String = s"Computation($name)"
  def report(colName: String, f: T => String) = new Computation(name,dependencies)(computation){
    /** The columns produced by this node. */
    override def columns: Seq[(String, T => String)] = outer.columns :+ (colName, f)
  }
}

object Computation {
  import shapeless._
  import shapeless.syntax.std.product._
  import shapeless.syntax.std.function._
  import shapeless.ops.function._
  def apply[DN <: HList, D <: HList, T, TupN <: Product, FunDT](name: String, dependencies: TupN)
                                                               (computation: FunDT)
                                                               (implicit
                                                                tupToHlist: Generic.Aux[TupN, DN],
                                                                unwrap: Comapped.Aux[DN, ValuedNode, D],
                                                                lub: ToTraversable.Aux[DN, List, Node],
                                                                fromT: FromTraversable[D],
                                                                fnUnHLister: FnToProduct.Aux[FunDT, D => T]): Computation[DN,D,T] = {
    new Computation(name, dependencies.toHList)(computation.toProduct)
  }
}