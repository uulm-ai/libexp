package exp.stages

import exp.Val
import exp.cli.CliOpt

import scalaz.{~>, Apply}

/**
  * Created by thomas on 23.11.15.
  */
case class CliEval[In[+_]]()(implicit val innerLift: LiftStream[In], val innerApply: Apply[In]) extends Stage {
  import scalaz.syntax.validation._

  type Read = Array[String]

  override type Inner[+T] = In[T]

  case class CliNode[+T](parser: CliOpt[In[T]]) extends Inject[T]

  override def processInject(r: Array[String], n: N[_]): Val[~>[Inject, In]] = new ~>[Inject, In]{
    override def apply[A](fa: Inject[A]): In[A] = {
      //takes a bit more work, we need to collect all nodes and process them at once
      ???
    }
  }.successNel

  object ProvidedInstances {
    implicit object cliNodeInst extends CLINode[N,In] {
      override def cliNode[T](co: CliOpt[In[T]]): N[T] = CliNode(co)
    }
  }
}
