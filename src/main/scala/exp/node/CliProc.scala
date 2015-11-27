package exp.node

import exp.cli._

import scalaz._
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.traverse._

/**
  * Created by thomas on 26.11.15.
  */
case object CliProc extends Stage { outer =>
  override type Payload[T] = CLI[Node[RngInsertion.type,T]]

  def cliOpt[T](co: CLI[Node[RngInsertion.type,T]]): Inject[CliProc.type, T] = Inject[CliProc.type,T](this,co)

  implicit def aboveRng: StageAfter[RngInsertion.type,CliProc.type] =
    StageAfter[RngInsertion.type,CliProc.type](CliProc)

  //we have to provide this because I can't get inference of transitivity to work, *sigh*
  implicit def fromBaseCast: StageCast[Base.type,CliProc.type] = new StageCast[Base.type,CliProc.type]{
    override def toStage: CliProc.type = outer
    override def apply[X](n: Node[Base.type, X]): Node[CliProc.type, X] = aboveRng.wrap(RngInsertion.aboveBase.wrap(n))
  }

  def createCLI[T](inputNode: Node[CliProc.type,T]): CLI[Node[RngInsertion.type,T]] = {


    val allCliNodes: List[(Inject[Stage, Any], CLI[Node[RngInsertion.type, Any]])] = inputNode.allNodes.collect{
      case n@Inject(CliProc,payload,None) => (n.asInstanceOf[Inject[Stage,Any]],payload.asInstanceOf[Payload[Any]])
    }.toList

    val cli = allCliNodes.map(_._2).sequenceU

    cli.map{ parsedNodes =>
      val map: Map[Inject[Stage, Any], Node[RngInsertion.type,_]] = allCliNodes.map(_._1).zip(parsedNodes).toMap

      def procR[X](node: Node[CliProc.type, X]): Node[RngInsertion.type, X] =
        node match {
          case i: Inject[Stage,X] => map(i.asInstanceOf[Inject[Stage,Any]]).asInstanceOf[Node[RngInsertion.type,X]]
          case Wrap(CliProc, n) => n.asInstanceOf[Node[RngInsertion.type, X]]
          case App(CliProc, ins, f, e, n) => App(RngInsertion, ins.map(procR[Any]), f, e, n)
          case Lift(CliProc, p, pie, el, n) => Lift(RngInsertion, procR(p), pie, el, n)
          case Report(CliProc, n, cn, f) => Report(RngInsertion, procR(n), cn, f.asInstanceOf[X => String])
          case otherwise => sys.error(s"inexhaustive match in RngInsertion on node $otherwise")
        }

      procR(inputNode)
    }
  }
}

