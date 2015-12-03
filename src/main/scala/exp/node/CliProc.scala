package exp.node

import exp.cli._

import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.~>

/** Stage that allows the insertion of nodes that are produced from command-line arguments.
  */
case object CliProc extends Stage { outer =>
  override type Payload[T] = (CliOpt[Node[S,T]], StageCast[S,RngInsertion.type]) forSome {type S <: Stage}

  type StageNode[+T] = Node[CliProc.type,T]

  def cliOpt[S <: Stage, T](co: CliOpt[Node[S,T]])(implicit cast: StageCast[S,RngInsertion.type]): Inject[CliProc.type, T] =
    Inject[CliProc.type,T](this,(co,cast),Some(co.long))

  implicit def aboveRng: StageAfter[RngInsertion.type,CliProc.type] =
    StageAfter[RngInsertion.type,CliProc.type](CliProc)

  //we have to provide this because I can't get inference of transitivity to work, *sigh*
  implicit def fromBaseCast: StageCast[Base.type,CliProc.type] = new StageCast[Base.type,CliProc.type]{
    override def toStage: CliProc.type = outer
    override def apply[X](n: Node[Base.type, X]): Node[CliProc.type, X] = aboveRng.wrap(RngInsertion.aboveBase.wrap(n))
  }

  def runStage(parseResult: Inject[CliProc.type,_] => Node[RngInsertion.type,_]): StageNode ~> RngInsertion.StageNode =
    new ~>[StageNode,RngInsertion.StageNode]{ inner =>
      override def apply[A](fa: StageNode[A]): RngInsertion.StageNode[A] = fa match {
        case i: Inject[CliProc.type,A] => parseResult(i).asInstanceOf[Node[RngInsertion.type,A]]
        case Wrap(CliProc, n) => n.asInstanceOf[Node[RngInsertion.type, A]] //validity of this cast is ensured by the existence of StageAfter values
        case App(CliProc, ins, f, e, n) => App(RngInsertion, ins.map(inner.apply[Any]), f, e, n)
        case Lift(CliProc, p, pie, el, n) => Lift(RngInsertion, inner.apply(p), pie, el, n)
        case Report(CliProc, n, cn, f) => Report(RngInsertion, inner.apply(n), cn, f.asInstanceOf[A => String])
        case otherwise => sys.error(s"inexhaustive match in CliProc on node $otherwise")
      }
    }

  def createCLI[T](inputNode: Node[CliProc.type,T]): CLI[Node[RngInsertion.type,T]] = {

    val allCliNodes: List[(Inject[CliProc.type, Any], Payload[_])] = inputNode.allNodes.collect{
      case n@Inject(CliProc,payload,_) => (n.asInstanceOf[Inject[CliProc.type,Any]],payload.asInstanceOf[Payload[Any]])
    }.toList

    allCliNodes.map(_._2)
      .map{case (co,cast) => liftCliOpt(co).map(cast.apply(_))}
      .sequenceU
      .map{ parsedNodes =>
        val map: Map[Inject[CliProc.type,_], Node[RngInsertion.type,_]] = allCliNodes.map(_._1).zip(parsedNodes).toMap
        runStage(map).apply(inputNode)
      }
  }
}

