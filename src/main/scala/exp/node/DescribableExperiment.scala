package exp.node

/** Type-class for experiments that can name their inputs, outputs and their computation graph.
  */
trait DescribableExperiment[A] {
  def inputs(a: A): Seq[IVariable]
  def outputs(a: A): Seq[DVariable]
  def experimentHash(a: A): Long
}

case class IVariable(name: String, inputType: Option[String], description: Option[String])
case class DVariable(name: String, variableType: Option[String], description: Option[String])