package exp.computation

sealed trait CEdge {
  def ins: IndexedSeq[CEdge]
  def name: String
}

object CEdge {
  def fromSeq[T](xs: Seq[T], name: String) = CedgeND(IndexedSeq(), name, _ => xs.toStream, xs.length, 0.00001d, 0.000001d)
}
case class CedgeND(ins: IndexedSeq[CEdge], name: String, f: IndexedSeq[_] => Stream[Any], estimatedLength: Double, estimatedCPUInit: Double, estimatedCPUExpand: Double) extends CEdge
case class CedgeDet(ins: IndexedSeq[CEdge], name: String, f: IndexedSeq[_] => Any, estimatedCPU: Double) extends CEdge
