package exp.node

import exp.computation._

/**
  * Created by thomas on 26.11.15.
  */
case object Base extends Stage {
  override type Payload[+T] = (IndexedSeq[T], String)
  override type This = Base.type
  override def nextStage: Nothing = sys.error("there's no stage after base")
  override type Next = Nothing

  def fromSeq[T](xs: Seq[T], name: String): Inject[Base.type, T] = Inject[Base.type,T](xs.toIndexedSeq -> name)

  /** Removes all `[[exp.node.Report]]` nodes from the graph and yields a sequence of columns replacing them. */
  def stripReport[T](node: Node[Base.type,T]): (Node[Base.type,T], List[(Node[Base.type,_], String, Any => String)]) = node match {
    case i@Inject(payload,_) =>
      (i, Nil)
    case Report(wrapped,name,f) =>
      val (stripped, columns) = stripReport(wrapped)
      (stripped, (stripped, name, f.asInstanceOf[Any => String]) :: columns)
    case l@Lift(p,_,_,_) =>
      val (stripped,columns) = stripReport(p)
      (l.copy(p = stripped), columns)
    case a@App(ins,_,_,_) =>
      val (stripped,columns) = (ins map stripReport[Any]).unzip
      (a.copy(inputs = stripped), columns.toList.flatten)
    case Wrap(wrapped) =>
      sys.error("found invalid node: " + node)
  }

  def toCGraph(node: Node[Base.type,_]): CGraph = {
    val (stripped, columns) = stripReport(node)
    def convert(n: Node[Base.type,_], acc: Map[Node[Base.type,_], CNode]): Map[Node[Base.type,_],CNode] =
      if(acc.contains(n)) acc
      else n match {
        case Inject(payload,_) =>
          val (xs: IndexedSeq[Any], name) = payload
          acc + (n -> CNode.fromSeq(xs,name))
        case Lift(p,perItem,expectedLength,nameOpt) =>
          val newMap = convert(p,acc)
          val liftNode = CedgeND(
            ins = IndexedSeq(newMap(p)),
            name = nameOpt.getOrElse("anon"),
            f = ins => ins(0).asInstanceOf[Stream[_]],
            estimatedLength = expectedLength.meanLength,
            estimatedCPUInit = 0.00001d,
            estimatedCPUExpand = perItem.expectedTime)
          newMap + (n -> liftNode)
        case App(ins,f,effort,nameOpt) =>
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
