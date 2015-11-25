package exp.stages

import exp.Context
import exp.cli.CliOpt

import scalaz.{~>, Apply}

/**
  * Created by thomas on 23.11.15.
  */
object StdStack{ outer =>
  import Final.ProvidedInstances._

  // type hierarchy
  //Base = Final.N -> WRng -> WCli -> WCliC = N
  //                       \> WRngC

  type Base[+T] = Final.N[T]

  val rngStack: RngInsertion[Final.N] =
    RngInsertion[Final.N]()

  import rngStack.ProvidedInstances._

  type WRng[+T] = rngStack.N[T]
  type WRngC[+T] = Context[T,WRng]

  val cliStack: CliEval[WRng] =
    CliEval[WRng]()(rngStack.N.liftStreamInst, rngStack.N.applyInstance)
  type WCli[+T] = cliStack.N[T]
  type WCliC[+T] = Context[T,cliStack.N]

  type N[+T] = WCliC[T]

  def applyCSN
  : Apply[cliStack.N] = cliStack.N.applyInstance
  def applyInst
  : Apply[N] = Context.applyInstance[cliStack.N](applyCSN)
  def liftStreamInst
  : LiftStream[N] = Context.liftStreamInstance[cliStack.N](cliStack.N.liftStreamInst)//implicitly[LiftStream[N]]
  implicit def fsAC
  : FromSeq[WRng] = FromSeq.liftInstance[Final.N,rngStack.N](rngStack.N.liftTransformation, Final.ProvidedInstances.fromSeqInt)
  implicit def fsACC
  : FromSeq[WRngC] = FromSeq.liftInstance[WRng,WRngC]
  implicit val acc2cli
  : WRng ~> cliStack.N = cliStack.N.liftTransformation
  def fsCSN
  : FromSeq[cliStack.N] = FromSeq.liftInstance[WRng, cliStack.N]
  def fromSeqInst
  : FromSeq[N] = FromSeq.liftInstance[cliStack.N,N](Context.wrapInstance[cliStack.N], fsCSN)
  implicit def rng2N: WRng ~> N = implicitly[WCli ~> WCliC] compose implicitly[WRng ~> WCli]
  def getSeedInst: GetSeed[N] = GetSeed.liftInstance[rngStack.N,N]

  object Ops {
    def fromSeq[T](xs: Seq[T], name: String): N[T] = fromSeqInst.fromSeq(xs, name)
    def getSeed(name: String): N[Long] = getSeedInst.getSeed(name).withName(s"seed.$name")
    def cliNode[T](opt: CliOpt[WRngC[T]]): N[T] = Context(cliStack.ProvidedInstances.cliNodeInst.cliNode(opt))

    implicit class Pimp[T](n: N[T]) {
      def lift[S](implicit ev: T <:< Stream[S]): N[S] = liftStreamInst.liftStream(n.asInstanceOf[N[Stream[S]]])
    }

    implicit def applyInst: Apply[N] = outer.applyInst
  }
}
