package exp.computation

/** A computation graph consists of a set of sink nodes and reports.
  * The sink nodes implicitly specify the full computation graph for a non-deterministic computation.
  *
  * Created by thomas on 26.11.15.
  */
case class CGraph(sinkNodes: Set[CNode], reports: Seq[Column]){
  /** The set of all nodes transitively referenced by the `sinkNodes`, including `sinkNodes`. */
  lazy val nodeClosure: Set[CNode] = exp.graphClosure(sinkNodes)(_.ins)
}
