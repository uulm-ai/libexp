package exp.computation

/**
  * Created by thomas on 26.11.15.
  */
case class CGraph(sinkNodes: Set[CEdge], reports: Seq[Column]){
  /** The set of all nodes transitively referenced by the `sinkNodes`, including `sinkNodes`. */
  lazy val nodeClosure: Set[CEdge] = exp.graphClosure(sinkNodes)(_.ins)
}
