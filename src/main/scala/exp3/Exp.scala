/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import org.parboiled2._
import scopt.{Read, OptionParser}
import shapeless.HList._
import shapeless.ops.hlist.{Comapped, ToTraversable}
import shapeless.ops.traversable.FromTraversable
import shapeless.syntax.std.traversable._
import shapeless.{:: => :::, HList, HNil}
import shapeless.syntax.typeable._

import scala.language.reflectiveCalls
import scala.util.{Try, Random}
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
  def nd: Rule1[NonDeterminism[T]]
  def Digits: Rule[HNil, HNil] = rule { oneOrMore(CharPredicate.Digit) }
}

/** An InputNode has is a independent variable.
  * It has no dependencies and can be fed compatible values to trigger computations in the successor nodes. */
trait InputNode[T] extends ValuedNode[T] {
  def fixed: Boolean
  def default: T
  def parser(s: ParserInput): NDParser[T]
  def defaultND: NonDeterminism[T] = Fixed(default)

  def parse(s: String): Try[NonDeterminism[T]] = {
    val p = parser(s)
    p.nd.run()
  }

  /** Installs a handler for the current InputNode within a scopt CLI-Parser. */
  def install(parser: OptionParser[Map[Node, NonDeterminism[_]]]): Unit = {
    implicit val tReader: Read[NonDeterminism[T]] = Read.reads(s => parse(s).get)
    parser
      .opt[NonDeterminism[T]](name)
      .action{case (si,m) => m + (this -> si)}
      .text(s"$name; default is $default" + (if(fixed) "; the value is fixed" else ""))
  }
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
                                                   val fromT: FromTraversable[D]) extends ComputedNode[DN,D,T] {
  override def toString: String = s"Computation($name)"
}

object Experiment {
  /** print the csv */
  def run(nodes: Set[Node], args: Seq[String]): Unit = {
    val graph = buildGraph(nodes)
    println(s"graph: $graph")
    val inputs: Iterable[InputNode[_]] = graph.keys.collect{case in: InputNode[_] => in}
    println(s"inputs: $inputs")
    val p  = parser(inputs.toSet)
    println(p.showUsage)
    val parseResult = p.parse(args,inputs.map(i => i -> i.defaultND).toMap)
    println(parseResult)

    parseResult.foreach(simpleDriver(graph,_))
  }

  def parser(inputs: Set[InputNode[_]]) = {
    val p = new OptionParser[Map[Node,NonDeterminism[_]]]("foo"){
      head("foo", "v0.0")
    }
    inputs.foreach(_.install(p))
    p
  }

  def buildGraph(nodes: Set[Node]): Map[Node,Set[Node]] = {
    def preds(n: Node): Set[Node] = n match {
      case _: InputNode[_] => Set()
      case c: Computation[_,_,_] => c.predecessors.toSet
    }
    val closure: Set[Node] =
      Stream.iterate(nodes)(s => s.flatMap(preds) ++ s).sliding(2).dropWhile(x => x.head.size != x.tail.head.size).next().head
    closure.map(n => n -> preds(n))(collection.breakOut)
  }

  def simpleDriver(graph: Map[Node,Set[Node]], nondet: Map[Node,NonDeterminism[_]]): Unit = {
    val to: Seq[Node] = topo(graph)
    println(s"topological ordering:\n\t$to")

    val firstValuation: Map[Node, Any] = nondet.map{case (n,Fixed(x)) => n -> x}

    def evaluate(valuation: Map[Node,Any]): Map[Node,Any] = {
      to.foldLeft(valuation){
        case (m,_:InputNode[_])      => m
        case (m,cn: ComputedNode[_,_,_]) =>
          val args: Seq[Any] = cn.predecessors.map(m)
          val result = cn.compute(args)
          m + (cn -> result)

      }
    }

    println(evaluate(firstValuation))
  }

  /** Construct a topological ordering of the given graph.
    * @param graph Maps a node to the set of its parents. */
  def topo[A](graph: Map[A,Set[A]], acc: Seq[A] = Seq()): Seq[A] =
    graph.collect{case (k,parents) if !acc.contains(k) && parents.forall(acc.contains) => k}.headOption match {
      case None => acc
      case Some(n) => topo(graph, acc :+ n)
    }

}
object Test {
  case class Problem(w: Int, s: Long)
  def main(args: Array[String]) {
    val width: ValuedNode[Int] = IntP("width", 2)
    val seed: ValuedNode[Long] = LongP("seed", 0)
    val problem = Computation("problem", width :: seed :: HNil)({
      x => Problem(x.head, x.tail.head)
    }: Int ::: Long ::: HNil => Problem)
    Experiment.run(Set(problem), args)
  }
}