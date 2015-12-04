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
    Inject[CliProc.type,T]((co,cast),Some(co.long))

  def createCLI[T](inputNode: Node[CliProc.type,T]): CLI[Node[RngInsertion.type,T]] = {

    val allCliNodes: List[Inject[CliProc.type, Any]] = inputNode.allNodesOnStage
      .collect{ case n@Inject(payload,_) => n }.distinct.toList

    allCliNodes.map(_.p)
      .map{case (co,cast) => liftCliOpt(co).map(cast.apply(_))}
      .sequenceU
      .map{ parsedNodes =>
        val map: Map[Inject[CliProc.type,_], Node[RngInsertion.type,_]] = allCliNodes.zip(parsedNodes).toMap
        val trans = new ~>[InjectNode,NextNode]{
          override def apply[A](fa: InjectNode[A]): NextNode[A] = map(fa).asInstanceOf[NextNode[A]]
        }
        runStage(trans).apply(inputNode)
      }
  }
}

