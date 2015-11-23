package exp.stages

import exp.Val

import scalaz.{~>, Apply}
import scalaz.syntax.validation._

/**
  * Created by thomas on 23.11.15.
  */
case class RngInsertion[In[+_]](inApply: Apply[In], inFS: FromSeq[In], inLift: LiftStream[In]) extends Stage{
  type Read = Seq[Long]
  type Inner[+T] = In[T]
  val innerFromSeq: FromSeq[In] = inFS

  override implicit def innerLift: LiftStream[In] = inLift

  override implicit def innerApply: Apply[In] = inApply

  case class RNGSeed(name: String) extends Inject[Long]

  implicit def getSeedInstance: GetSeed[N] = new GetSeed[N]{
    override def getSeed(name: String): N[Long] = RNGSeed(name)
  }

  override def processInject(r: Seq[Long]): Val[~>[Inject, In]] = new ~>[Inject, In] {
    override def apply[A](fa: Inject[A]): In[A] = fa match {
      case RNGSeed(name) => innerFromSeq.fromSeq(r)
    }
  }.successNel
}
