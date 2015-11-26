package exp.computation

/** Non-Optimizing, single-threaded evaluator. */
object SimpleEvaluator {

  case class Valuation(assignment: Map[CEdge,Any]){
    def apply(n: CEdge): Any = assignment(n)
    def +(entry: (CEdge,Any)): Valuation = Valuation(assignment + entry)

    override def toString: String = s"Val(${assignment.map{case (k,v) => s"${k.name}:$v"}.mkString(";")})"
  }

  def evalStream(computation: CGraph): Stream[Valuation] = {

    def topoSort(remaining: Set[CEdge], acc: List[CEdge]): List[CEdge] = {
      val open = remaining.filterNot(r => r.ins.exists(remaining))
      if(open.isEmpty){
        require(remaining.isEmpty, "computation graph contains a directed cycle")
        acc.reverse
      } else {
        val n = open.head
        topoSort(remaining - n, n :: acc)
      }
    }

    val topoOrder: List[CEdge] = topoSort(computation.nodeClosure, Nil)

    def evaluate(n: CEdge, valuation: Valuation): Stream[Valuation] = n match {
      case CedgeND(ins,_,f,_,_,_) =>
        f(ins.map(valuation.apply)).map(x => valuation + (n -> x))
      case CedgeDet(ins,_,f,_)    =>
        Stream(valuation + (n -> f(ins map valuation.apply)))
    }

    topoOrder.foldLeft(Stream(Valuation(Map()))){case (vs,node) =>
        vs.flatMap(v => evaluate(node,v))
    }
  }
}
