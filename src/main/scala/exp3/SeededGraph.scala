/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp3

import java.io.OutputStream
import java.text.DateFormat
import java.util.Date
import java.util.concurrent.atomic.AtomicLong

import scopt.OptionParser
import vultura.util.DomainCPI

import scala.util.Random

case class Experiment(name: String, nodes: Set[Node])

case class SeededGraph(graph: Map[Node,Set[Node]], inputND: Map[InputNode[_],NonDeterminism[_]])

case class RunConfig(numSeeds: Long = 5,
                     startSeed: Long = 0,
                     runParallel: Boolean = true,
                     reportIntervall: Option[Int] = None){
  def baseSeeds: Iterable[Long] = startSeed until (startSeed + numSeeds)
}
trait Output {
  def out: OutputStream
}
/** Run the experiment. */
case class ExperimentCSV(out: OutputStream, runConfig: RunConfig)
/** Output a dot version of the computation. */
case class ComputationGraph(out: OutputStream)

case class ExpApp(name: String = "expapp", appversion: String = "0.0", description: String = "") {
  /** print the csv */
  def run(nodes: Set[Node], args: Seq[String]): Unit = {
    val graph = buildGraph(nodes)
    val inputs: Iterable[InputNode[_]] = graph.keys.collect{case in: InputNode[_] => in}
    val p  = parser(inputs.toSet)
    val parseResult = p.parse(args,RunConfig() -> inputs.map(i => i -> i.default).toMap)
    parseResult.foreach { case (rc,nd) =>
      val (colNames, rows) = simpleDriver(SeededGraph(graph, nd), rc)
      println(colNames.mkString("\t"))
      rows.map(_.mkString("\t")).foreach(println)
    }
  }

  def parser(inputs: Set[InputNode[_]]) = {
    val p = new OptionParser[(RunConfig,Map[InputNode[_],NonDeterminism[_]])](name){
      head(name, appversion)
      opt[Int]('n',"num-samples")
        .action{case (n, (rc,m)) => (rc.copy(numSeeds = n),m)}
      opt[Int]("print-progress")
        .action{case (n, (rc,m)) => (rc.copy(reportIntervall = Some(n)),m)}
        .text("time intervall in seconds after which to print progress on stderr; leave unspecified for no progress report")
    }
    inputs.foreach(_.install(p))
    p
  }

  def buildGraph(nodes: Set[Node]): Map[Node,Set[Node]] = {
    def preds(n: Node): Set[Node] = n match {
      case _: InputNode[_] => Set()
      case c: UntypedComputation[_] => c.predecessors.toSet
    }
    val closure: Set[Node] =
      Stream.iterate(nodes)(s => s.flatMap(preds) ++ s).sliding(2).dropWhile(x => x.head.size != x.tail.head.size).next().head
    closure.map(n => n -> preds(n))(collection.breakOut)
  }

  /** Generate all full-factorial assignments to the [[exp3.Stratification]] nodes, 
    * sampling the [[exp3.Distribution]] nodes. */
  def assignmentSequence(nds: Seq[NonDeterminism[_]], seed: Long): Iterator[Seq[Any]] = {
    val indexToNode: Map[Int, NonDeterminism[_]] = nds.zipWithIndex.map(_.swap).toMap
    val strats: Seq[Stratification[_]] = nds.collect{case strat: Stratification[_] => strat}
    val values: Seq[Seq[Any]] = strats.map(_.values)
    val counter = new DomainCPI(values.map(_.toArray).toArray)
    counter.iterator.map { stratValues =>
      val stratMap = (strats zip stratValues).toMap
      val rnd = new Random(seed)
      val rngs = Stream.continually(rnd.nextLong()).map(new Random(_))
      (nds zip rngs).map {
        case (s: Stratification[_],_)    => stratMap(s)
        case (Distribution(sample),r) => sample(r)
      }
    }
  }

  /** Naive driver that uses a topological ordering, breaking ties based on node name. */
  def simpleDriver(exp: SeededGraph, rc: RunConfig): (Seq[String],Seq[Seq[String]]) = {
    val to: Seq[Node] = topologicalOrdering(exp.graph,Ordering.by((_:Node).name))
    val columns: Seq[(ValuedNode[_], Seq[(String, Nothing => String)])] =
      to.collect{ case vn: ValuedNode[_] if vn.columns.nonEmpty => vn -> vn.columns}
    val colNames: Seq[String] = columns.flatMap(_._2.map(_._1))

    val ins = to.collect{case in: InputNode[_] => in}

    val assignments: IndexedSeq[(Long,Seq[Any])] =
      Random.shuffle(rc.baseSeeds.iterator.flatMap(bs => assignmentSequence(ins.map(exp.inputND), bs).map(bs -> _)).toIndexedSeq)

    val numTasks: Int = assignments.size

    val startTime = System.nanoTime()
    val lastReport: AtomicLong = new AtomicLong(startTime)
    val numProcessed: AtomicLong = new AtomicLong(0L)
    def evaluate(valuation: Map[Node,Any]): Map[Node,Any] = {
      val result = to.foldLeft(valuation){
        case (m,_:InputNode[_])      => m
        case (m,cn: UntypedComputation[_]) =>
          val args: Seq[Any] = cn.predecessors.map(m)
          val result = cn.compute(args)
          m + (cn -> result)
      }

      val now = System.nanoTime()
      val procNumber = numProcessed.incrementAndGet()
      rc.reportIntervall.foreach{intervall =>
        this.synchronized {
          if(now >= lastReport.get() + intervall*1e9){
            val ratio = procNumber/numTasks.toDouble
            val dateString: String = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT).format(new Date())
            val duration = (now - startTime) * 1e-9
            val eta = duration/ratio * (1-ratio)
            System.err.println(f"$dateString - progress ${ratio*100}%.2f%% ($procNumber/$numTasks) - ETA ${eta/60}%.0f min")
            lastReport.set(now)
          }
        }
      }

      result
    }

    def buildRow(valuation: Map[Node,Any]): Seq[String] = columns.flatMap{ case (node, cols) =>
      val value = valuation(node)
      cols.map(_._2.asInstanceOf[Any => String](value))
    }

    val evaluations: Iterable[(Long,Map[Node, Any])] = if(rc.runParallel)
      assignments.toSeq.par.map{case (bs,vals) => bs -> evaluate((ins zip vals).toMap)}.seq
    else assignments.map{case (bs,vals) => bs -> evaluate((ins zip vals).toMap)}.toIterable

    val csvRows: Iterable[Seq[String]] = evaluations.map{case (bs,eval) => bs.toString +: buildRow(eval)}
    ("base.seed" +: colNames, csvRows.toSeq)
  }

  /** Construct a topological ordering of the given graph.
    * @param graph Maps a node to the set of its parents. */
  def topologicalOrdering[A](graph: Map[A,Set[A]],
                             break: Ordering[A],
                             acc: Seq[A] = Seq()): Seq[A] =
    graph.collect{case (k,parents) if !acc.contains(k) && parents.forall(acc.contains) => k}
      .toSeq.sorted(break).headOption match {
      case None => acc
      case Some(n) => topologicalOrdering(graph, break, acc :+ n)
    }
}
