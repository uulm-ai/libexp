package exp.stages

import exp.Context
import exp.cli.CliOpt

import scalaz.Apply

/**
  * Created by thomas on 23.11.15.
  */
object StdStack{ outer =>
  val rngStack: RngInsertion[Final.N] = RngInsertion[Final.N](Final.applyInstance, Final.fromSeqIns, Final.liftStreamInst)
  type AfterCli[+T] = rngStack.N[T]
  val cliStack: CliEval[rngStack.N] = CliEval[rngStack.N](rngStack.liftStreamInst,rngStack.applyInstance)
  type N[+T] = Context[T,cliStack.N]

  implicit def applyInst: Apply[N] = implicitly[Apply[N]]
  implicit def liftStreamInst: LiftStream[N] = implicitly[LiftStream[N]]
  implicit def fromSeqInst: FromSeq[N] = implicitly[FromSeq[N]]
  implicit def getSeedInst: GetSeed[N] = implicitly[GetSeed[N]]
  implicit def cliNodeInst: CLINode[N,AfterCli] = implicitly[CLINode[N,AfterCli]]


  object Ops {
    def fromSeq[T](xs: Seq[T]): N[T] = fromSeqInst.fromSeq(xs)
    def getSeed(name: String): N[Long] = getSeedInst.getSeed(name).withName(s"seed.$name")
    def cliNode[T](opt: CliOpt[AfterCli[T]]): N[T] = cliNodeInst.cliNode(opt)

    implicit class Pimp[T](n: N[T]) {
      def lift[S](implicit ev: T <:< Stream[S]): N[S] = liftStreamInst.liftStream(n.asInstanceOf[N[Stream[S]]])
    }

    implicit def applyInst: Apply[N] = outer.applyInst
  }
}
