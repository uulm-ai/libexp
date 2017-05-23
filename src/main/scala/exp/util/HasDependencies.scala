package exp.util

import exp.computation.CNode

trait HasDependencies[A] {def dependencies(a: A): Seq[A]}

object HasDependencies {
  def apply[A: HasDependencies]: HasDependencies[A] = implicitly[HasDependencies[A]]
  implicit def instCNode = new HasDependencies[CNode] {
    override def dependencies(a: CNode): Seq[CNode] = a.ins
  }
}