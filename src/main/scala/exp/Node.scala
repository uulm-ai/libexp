package exp

import exp.stages.Stage

import scala.language.higherKinds

/** A node is a term evaluating to type T.
  * The type member `S` defines the stage in which it will get evaluated. */
trait Node[+T] {
  /** Indicates which pre-processing steps are necessary for this node. */
  type S <: Stage
}
