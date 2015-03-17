/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import scopt.OptionParser
import vultura.util.DomainCPI

import scala.util.Random

case class Experiment(graph: Map[Node,Set[Node]], inputND: Map[InputNode[_],NonDeterminism[_]])

object Experiment {
  /** print the csv */
  def run(nodes: Set[Node], args: Seq[String]): Unit = {
    val graph = buildGraph(nodes)
    println(s"graph: $graph")
    val inputs: Iterable[InputNode[_]] = graph.keys.collect{case in: InputNode[_] => in}
    println(s"inputs: $inputs")
    val p  = parser(inputs.toSet)
    println(p.showUsage)
    val parseResult = p.parse(args,inputs.map(i => i -> i.default).toMap)
    println(parseResult)

    parseResult.foreach { nd =>
      val (colNames, rows) = simpleDriver(Experiment(graph, nd), new Random(0))
      println(colNames.mkString("\t"))
      rows.map(_.mkString("\t")).foreach(println)
    }
  }

  def parser(inputs: Set[InputNode[_]]) = {
    val p = new OptionParser[Map[InputNode[_],NonDeterminism[_]]]("foo"){
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

  def assignmentSequence(nds: Seq[NonDeterminism[_]], random: Random): Iterator[Seq[Any]] = {
    val indexToNode: Map[Int, NonDeterminism[_]] = nds.zipWithIndex.map(_.swap).toMap
    val strats: Seq[Stratification[_]] = nds.collect{case strat: Stratification[_] => strat}
    val values: Seq[Seq[Any]] = strats.map(_.values)
    val counter = new DomainCPI(values.map(_.toArray).toArray)
    counter.iterator.map { stratValues =>
      val stratMap = (strats zip stratValues).toMap
      nds.map {
        case s: Stratification[_]    => stratMap(s)
        case Fixed(x)                => x
        case Distribution(sample) => sample(random)
      }
    }

  }

  def simpleDriver(exp: Experiment, random: Random): (Seq[String],Seq[Seq[String]]) = {
    val to: Seq[Node] = topo(exp.graph)
    val columns: Seq[(ValuedNode[_], Seq[(String, Nothing => String)])] =
      to.collect{ case vn: ValuedNode[_] if vn.columns.nonEmpty => vn -> vn.columns}
    val colNames: Seq[String] = columns.flatMap(_._2.map(_._1))
    println(s"topological ordering:\n\t$to")

    val ins = to.collect{case in: InputNode[_] => in}

    val assignments: Iterator[Seq[Any]] = assignmentSequence(ins.map(exp.inputND), random)

    def evaluate(valuation: Map[Node,Any]): Map[Node,Any] = {
      to.foldLeft(valuation){
        case (m,_:InputNode[_])      => m
        case (m,cn: ComputedNode[_,_,_]) =>
          val args: Seq[Any] = cn.predecessors.map(m)
          val result = cn.compute(args)
          m + (cn -> result)

      }
    }

    def buildRow(valuation: Map[Node,Any]): Seq[String] = columns.flatMap{ case (node, cols) =>
      val value = valuation(node)
      cols.map(_._2.asInstanceOf[Any => String](value))
    }

    val evaluations: Iterator[Map[Node, Any]] = assignments.map(vals => (ins zip vals).toMap: Map[Node,Any]).map(evaluate)
    val csvRows = evaluations.map(buildRow)
    (colNames, csvRows.toSeq)
  }

  /** Construct a topological ordering of the given graph.
    * @param graph Maps a node to the set of its parents. */
  def topo[A](graph: Map[A,Set[A]], acc: Seq[A] = Seq()): Seq[A] =
    graph.collect{case (k,parents) if !acc.contains(k) && parents.forall(acc.contains) => k}.headOption match {
      case None => acc
      case Some(n) => topo(graph, acc :+ n)
    }
}
