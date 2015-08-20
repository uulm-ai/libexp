/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import fastparse.P
import scopt.{OptionParser, Read}

import scala.language.reflectiveCalls


/** @tparam R result type of function type F
  * @tparam A argument product type (not wrapped as Node)
  */
trait ParList[A,R] extends Serializable {
  /** A wrapped in node product type. */
  type NA
  type F
  def listFunction(f: F): Seq[Any] => R
  def listDeps(deps: NA): Seq[Node]
}

object ParList{
  type Aux[A,R,Na,Fa] = ParList[A,R]{
    type NA = Na
    type F = Fa
  }
  implicit def t1[A,R]: ParList.Aux[A, R, ValuedNode[A], A => R] = new ParList[A,R]{
    override type NA = ValuedNode[A]
    override type F = A => R
    override def listDeps(deps: ValuedNode[A]): Seq[Node] = Seq(deps)
    override def listFunction(f: A => R): (Seq[Any]) => R = { xs =>
      f(xs.head.asInstanceOf[A])
    }
  }
  implicit def t2[X1,X2,R]: ParList.Aux[(X1, X2), R,(ValuedNode[X1],ValuedNode[X2]), (X1,X2) => R]  =
    new ParList[(X1,X2),R]{
      override type NA = (ValuedNode[X1],ValuedNode[X2])
      override type F = (X1,X2) => R
      override def listDeps(deps: (ValuedNode[X1], ValuedNode[X2])): Seq[Node] = Seq(deps._1,deps._2)
      override def listFunction(f: (X1, X2) => R): (Seq[Any]) => R = { xs =>
        val x1 = xs(0).asInstanceOf[X1]
        val x2 = xs(1).asInstanceOf[X2]
        f(x1,x2)
      }
    }
  implicit def t3[X1,X2,X3,R]: ParList.Aux[(X1,X2,X3), R, (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3]), (X1, X2, X3) => R]  =
    new ParList[(X1,X2,X3), R]{
      override type NA = (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3])
      override type F = (X1, X2, X3) => R
      override def listDeps(deps: (ValuedNode[X1], ValuedNode[X2], ValuedNode[X3])): Seq[Node] = Seq(deps._1,deps._2,deps._3)
      override def listFunction(f: (X1, X2, X3) => R): (Seq[Any]) => R = { xs =>
        val x1 = xs(0).asInstanceOf[X1]
        val x2 = xs(1).asInstanceOf[X2]
        val x3 = xs(2).asInstanceOf[X3]
        f(x1,x2,x3)
      }
    }
  implicit def t4[X1,X2,X3,X4,R]: ParList.Aux[(X1, X2, X3, X4), R, (ValuedNode[X1], ValuedNode[X2], ValuedNode[X3], ValuedNode[X4]), (X1, X2, X3, X4) => R]  =
    new ParList[(X1,X2,X3,X4),R]{

      override type NA = (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4])
      override type F = (X1, X2, X3, X4) => R

      override def listDeps(deps: (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4])): Seq[Node] = Seq(deps._1,deps._2,deps._3, deps._4)

      override def listFunction(f: (X1,X2,X3,X4) => R): (Seq[Any]) => R = { xs =>
        val x1 = xs(0).asInstanceOf[X1]
        val x2 = xs(1).asInstanceOf[X2]
        val x3 = xs(2).asInstanceOf[X3]
        val x4 = xs(3).asInstanceOf[X4]
        f(x1,x2,x3,x4)
      }
    }
  implicit def t5[X1,X2,X3,X4,X5,R]: ParList.Aux[(X1, X2, X3,X4,X5), R, (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5]), (X1, X2, X3,X4,X5) => R]  =
    new ParList[(X1, X2, X3, X4, X5), R]{
      override type NA = (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5])
      override type F = (X1, X2, X3,X4,X5) => R

      override def listDeps(deps: (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5])): Seq[Node] = Seq(deps._1,deps._2,deps._3, deps._4, deps._5)

      override def listFunction(f: (X1, X2, X3,X4,X5) => R): (Seq[Any]) => R = { xs =>
        val x1 = xs(0).asInstanceOf[X1]
        val x2 = xs(1).asInstanceOf[X2]
        val x3 = xs(2).asInstanceOf[X3]
        val x4 = xs(3).asInstanceOf[X4]
        val x5 = xs(4).asInstanceOf[X5]

        f(x1,x2,x3,x4,x5)
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

case class FixedInput[T](name: String, values: NonDeterminism[T]) extends ValuedNode[T]

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