package exp.stages

import exp.Val

import scalaz.{~>, Apply}
import scalaz.syntax.validation._

/**
  * Created by thomas on 23.11.15.
  */
case class RngInsertion[In[+_]]()(implicit val innerApply: Apply[In], val innerFromSeq: FromSeq[In], val innerLift: LiftStream[In]) extends Stage {
  type Read = Seq[Long]
  type Inner[+T] = In[T]

  case class RNGSeed(name: String) extends Inject[Long]


  override def processInject(r: Seq[Long], n: N[_]): Val[~>[Inject, In]] = new ~>[Inject, In] {
    override def apply[A](fa: Inject[A]): In[A] = fa match {
      case RNGSeed(name) => innerFromSeq.fromSeq(r, s"seed.$name")
    }
  }.successNel

  object ProvidedInstances {
    implicit object getSeedInstance extends GetSeed[N]{
      override def getSeed(name: String): N[Long] = RNGSeed(name)
    }
  }
}
