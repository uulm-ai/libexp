package exp.stages

import exp._
import exp.stages.cli.CLIParsing
import exp.stages.evaluation.Evaluation

trait RNGInsertion extends CLIParsing {
  type N[+A] <: Node[A] {type S <: RNGInsertion}
}
/**
  * Created by thomas on 20.11.15.
  */
object RNGInsertion extends Stage {
//  type I = Long
//  type Out = Context.WithAny[Evaluation#N]
//
//  def runStage(seed: Long): Val[(Context.WithAny[N]) => Context.WithAny[Evaluation#N]] = {
//    ???
//  }
//
  case object RNGSeed extends Node[Long]{
    type S = RNGInsertion.type
  }
}
