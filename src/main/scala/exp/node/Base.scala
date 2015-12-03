package exp.node

import exp.computation._

/**
  * Created by thomas on 26.11.15.
  */
case object Base extends Stage {
  override type Payload[+T] = (IndexedSeq[T], String)

  def fromSeq[T](xs: Seq[T], name: String): Inject[Base.type, T] =
    Inject[Base.type,T](this,xs.toIndexedSeq -> name)
  implicit def aboveNothing: StageAfter[Nothing,Base.type] = StageAfter(this)

  /** Removes all `[[exp.node.Report]]` nodes from the graph and yields a sequence of columns replacing them. */
  def stripReport[T](node: Node[Base.type,T]): (Node[Base.type,T], List[(Node[Base.type,_], String, Any => String)]) = node match {
    case i@Inject(Base,payload,_) =>
      (i, Nil)
    case Report(Base,wrapped,name,f) =>
      val (stripped, columns) = stripReport(wrapped)
      (stripped, (stripped, name, f.asInstanceOf[Any => String]) :: columns)
    case l@Lift(_,p,_,_,_) =>
      val (stripped,columns) = stripReport(p)
      (l.copy(p = stripped), columns)
    case a@App(_,ins,_,_,_) =>
      val (stripped,columns) = (ins map stripReport[Any]).unzip
      (a.copy(inputs = stripped), columns.toList.flatten)
    case Wrap(_,wrapped) =>
      sys.error("found invalid node: " + node)
    case Inject(_,_,_) =>
      sys.error("found invalid node: " + node)

  }

  def toCGraph(node: Node[Base.type,_]): CGraph = {
    val (stripped, columns) = stripReport(node)
    def convert(n: Node[Base.type,_], acc: Map[Node[Base.type,_], CEdge]): Map[Node[Base.type,_],CEdge] =
      if(acc.contains(n)) acc
      else n match {
        case Inject(Base,payload,_) =>
          val (xs: IndexedSeq[Any], name) = payload
          acc + (n -> CEdge.fromSeq(xs,name))
        case Lift(_,p,perItem,expectedLength,nameOpt) =>
          val newMap = convert(p,acc)
          val liftNode = CedgeND(
            ins = IndexedSeq(newMap(p)),
            name = nameOpt.getOrElse("anon"),
            f = ins => ins(0).asInstanceOf[Stream[_]],
            estimatedLength = expectedLength.meanLength,
            estimatedCPUInit = 0.00001d,
            estimatedCPUExpand = perItem.expectedTime)
          newMap + (n -> liftNode)
        case App(_,ins,f,effort,nameOpt) =>
          val updated = ins.foldLeft(acc) {case (m, nextNode) => convert(nextNode, m)}
          val newNode = CedgeDet(ins.map(updated),nameOpt.getOrElse("anon"),f,effort.expectedTime)
          updated + (n -> newNode)
        case otherwise =>
          sys.error("this should not happen")
      }
    val conversion = convert(stripped, Map())
    CGraph(Set(conversion(stripped)), columns.map{case (n,s,f) => Column(s,conversion(n),f)}.distinct)
  }
}
