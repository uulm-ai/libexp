package exp.stages.evaluation

import exp.Node
import exp.stages.Stage

/**
  * Created by thomas on 20.11.15.
  */
trait Evaluation extends Stage {
  type I = Unit
  type N[+A] = Node[A] {type S <: Evaluation}
}
