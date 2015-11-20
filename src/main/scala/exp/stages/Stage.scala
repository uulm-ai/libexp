package exp.stages

import exp._

import scala.language.higherKinds

trait Stage {
  type N[+A] <: Node[A]
  type I
  type Out
  def runStage(input: I): Val[ComputationGraph[N] => Out]
}
