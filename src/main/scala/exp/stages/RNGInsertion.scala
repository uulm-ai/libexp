package exp.stages

import exp._
import exp.stages.evaluation.Evaluation

/**
  * Created by thomas on 20.11.15.
  */
object RNGInsertion extends Stage {
  type I = Long
  type N[+A] = Node[A] {type S <: RNGInsertion.type}
  type Out = ComputationGraph[Evaluation#N]

  def runStage(seed: Long): Val[(ComputationGraph[N]) => ComputationGraph[Evaluation#N]] = ???

  case object RNGSeed extends Node[Long]{
    type S = RNGInsertion.type
  }
}
