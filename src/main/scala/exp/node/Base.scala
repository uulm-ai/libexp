package exp.node

/**
  * Created by thomas on 26.11.15.
  */
case object Base extends Stage {
  override type Payload[+T] = (IndexedSeq[T], String)

  def fromSeq[T](xs: Seq[T], name: String): Inject[Base.type, T] =
    Inject[Base.type,T](this,xs.toIndexedSeq -> name)
  implicit def aboveNothing: StageAfter[Nothing,Base.type] = StageAfter(this)
}


object SimpleEvaluator {
  type N = Node[Stage, _]

  case class Valuation(assignment: Map[N,Any]){
    def apply(n: N): Any = assignment(n)
    def +(entry: (N,Any)): Valuation = Valuation(assignment + entry)
  }

  def evalStream(n: Node[Base.type,_]): Stream[Valuation] = {
    val allNodes: Set[N] = Node.extractNodes(n)

    def topoSort(remaining: Set[N], acc: List[N]): List[N] = {
      val open = remaining.filterNot(r => r.dependencies.exists(remaining))
      if(open.isEmpty){
        require(remaining.isEmpty, "computation graph contains a directed cycle")
        acc.reverse
      } else {
        val n = open.head
        topoSort(remaining - n, n :: acc)
      }
    }

    val topoOrder: List[N] = topoSort(allNodes, Nil)

    def evaluate(n: N, valuation: Valuation): Stream[Valuation] = n match {
      case Inject(Base,payload) => payload.asInstanceOf[(IndexedSeq[_],String)]._1.toStream.map(x => valuation + (n -> x))
      case Inject(_,_) => sys.error("found invalid node: " + n)
      case Wrap(_,wrapped) => Stream(valuation + (n -> valuation(wrapped)))
      case Report(_,wrapped,_,_) => Stream(valuation + (n -> valuation(wrapped)))
      case Lift(_,p) => valuation(p).asInstanceOf[Stream[Any]].map(x => valuation + (n -> x))
      case App(_,ins,f,_) => Stream(valuation + (n -> f(ins.map(valuation.apply(_)))))
    }

    topoOrder.foldLeft(Stream(Valuation(Map()))){case (vs,n) =>
        vs.flatMap(v => evaluate(n,v))
    }
  }
}