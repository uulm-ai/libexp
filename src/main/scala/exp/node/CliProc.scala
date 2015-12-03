package exp.node

import exp.cli._

import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.~>

/** Stage that allows the insertion of nodes that are produced from command-line arguments.
  */
case object CliProc extends Stage { outer =>
  override type Payload[+T] = (CliOpt[Node[S,T]], StageCast[S,RngInsertion.type]) forSome {type S <: Stage}
  type This = CliProc.type
  override type Next = RngInsertion.type
  override def nextStage: CliProc.Next = RngInsertion

  def cliOpt[S <: Stage, T](co: CliOpt[Node[S,T]])(implicit cast: StageCast[S,RngInsertion.type]): Inject[CliProc.type, T] =
    Inject[CliProc.type,T](this,(co,cast),Some(co.long))

  implicit def aboveRng: StageAfter[RngInsertion.type,CliProc.type] =
    StageAfter[RngInsertion.type,CliProc.type](CliProc)

  //we have to provide this because I can't get inference of transitivity to work, *sigh*
  implicit def fromBaseCast: StageCast[Base.type,CliProc.type] = new StageCast[Base.type,CliProc.type]{
    override def toStage: CliProc.type = outer
    override def apply[X](n: Node[Base.type, X]): Node[CliProc.type, X] = aboveRng.wrap(RngInsertion.aboveBase.wrap(n))
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
        val trans = new ~>[InjectNode,NextNode]{
          override def apply[A](fa: InjectNode[A]): NextNode[A] = map(fa).asInstanceOf[NextNode[A]]
        }
        runStage(trans).apply(inputNode)
      }
  }
}

