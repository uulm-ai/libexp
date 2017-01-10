package exp.computation

/** A node within a computation graph.
  * It has a name and a list of input nodes.
  */
sealed trait CNode {
  def ins: IndexedSeq[CNode]
  def name: String
}

object CNode {
  def fromSeq[T](xs: Seq[T], name: String) = CedgeND(IndexedSeq(), name, _ => xs.toStream, xs.length, 0.00001d, 0.000001d)
  def pure[T](x: T, name: String) = CedgeDet(IndexedSeq(), name, _ => x, 0.00001d)
}

/** A non-deterministic computation node produces multiple output values for each set of input values. */
case class CedgeND(ins: IndexedSeq[CNode], name: String, f: IndexedSeq[_] => Stream[Any], estimatedLength: Double, estimatedCPUInit: Double, estimatedCPUExpand: Double) extends CNode
/** A deterministic computation node is basically a function mapping its inputs to a value. */
case class CedgeDet(ins: IndexedSeq[CNode], name: String, f: IndexedSeq[_] => Any, estimatedCPU: Double) extends CNode
