package exp.node

import exp._
import exp.cli.CliOpt

/**
  * Created by thomas on 26.11.15.
  */
case object CliProc extends Stage { outer =>
  override type Payload[+T] = CliOpt[Node[RngInsertion.type,T]]
  def cliOpt[T](co: CliOpt[Node[RngInsertion.type,T]]): Inject[CliProc.type, T] = Inject[CliProc.type,T](this,co)

  def process[T](args: Array[String], n: Node[CliProc.type,T]): Val[Node[RngInsertion.type,T]] = ???

  implicit def aboveRng: StageAfter[RngInsertion.type,CliProc.type] =
    StageAfter[RngInsertion.type,CliProc.type](CliProc)

  //we have to provide this because I can't get inference of transitivity to work, *sigh*
  implicit def fromBaseCast: StageCast[Base.type,CliProc.type] = new StageCast[Base.type,CliProc.type]{
    override def toStage: CliProc.type = outer
    override def apply[X](n: Node[Base.type, X]): Node[CliProc.type, X] = aboveRng.wrap(RngInsertion.aboveBase.wrap(n))
  }
}
