/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import org.parboiled2._
import scopt.{OptionParser, Read}
import shapeless.HNil

import scala.language.reflectiveCalls
import scala.util.Try


trait ParList[NT,R,FX] extends Serializable {
  def listFunction(f: FX): Seq[Any] => R
  def listDeps(deps: NT): Seq[Node]
}

object ParList{
  implicit def t1[X,R]: ParList[ValuedNode[X], R, (X) => R] = new ParList[ValuedNode[X],R,X => R]{

    override def listDeps(deps: ValuedNode[X]): Seq[Node] = Seq(deps)

    override def listFunction(f: X => R): (Seq[Any]) => R = { xs =>
      f(xs.head.asInstanceOf[X])

    }
  }
  implicit def t2[X1,X2,R]: ParList[(ValuedNode[X1], ValuedNode[X2]), R, (X1, X2) => R]  =
    new ParList[(ValuedNode[X1],ValuedNode[X2]),R,(X1,X2) => R]{

      override def listDeps(deps: (ValuedNode[X1], ValuedNode[X2])): Seq[Node] = Seq(deps._1,deps._2)

      override def listFunction(f: (X1, X2) => R): (Seq[Any]) => R = { xs =>
        val x1 = xs(0).asInstanceOf[X1]
        val x2 = xs(1).asInstanceOf[X2]
        f(x1,x2)
      }
    }
}

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


trait UntypedComputation[T] extends ValuedNode[T]{
  def predecessors: Seq[Node]
  def compute(args: Seq[Any]): T
}

case class UTC[T](name: String, computation: Seq[Any] => T, predecessors: Seq[Node], override val columns: Seq[(String, T => String)]) extends UntypedComputation[T] {
  override def compute(args: Seq[Any]): T = computation(args)
}

case class TypedComputation[NT,R,F](name: String,
                                    deps: NT,
                                    computation: F,
                                    pl: ParList[NT,R,F],
                                    override val columns: Seq[(String, R => String)]) extends UntypedComputation[R]{
  override def predecessors: Seq[Node] = pl.listDeps(deps)
  override def compute(args: Seq[Any]): R = pl.listFunction(computation)(args)
  def report(colName: String, f: R => String) = copy(columns = columns :+ (colName,f))
}

object TypedComputation{
  def apply[DN,F,R](name: String, dependencies: DN)(computation: F)(implicit pl: ParList[DN,R,F]): TypedComputation[DN, R, F] =
    new TypedComputation(name,dependencies,computation,pl,Seq())
}