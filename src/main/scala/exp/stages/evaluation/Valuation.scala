package exp.stages.evaluation

/** Assignment of values to nodes of the final stage.
  * Represents one (partial) branch of the non-deterministic computation. */
case class Valuation(map: Map[Evaluation#N[_],Any]) {
  def apply[T](node: Evaluation#N[T]): T = map(node).asInstanceOf[T]
  def +[T](kv: (Evaluation#N[T], T)): Valuation = Valuation(map + kv)
}
