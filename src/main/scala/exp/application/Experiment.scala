package exp.application

import exp.cli.CliOpt

/** An experiment is a sub-command that performs an experiment. */
case class Experiment(name: String, description: String, version: String, computation: exp.node.CliProc.StageNode[Any])
  extends Subcommand {
  override type T = this.type

  override def run: (T) => Unit = ???

  override def cli: CliOpt[T] = ???
}

trait Subcommand {
  /** Configuration type. */
  type T
  def name: String
  def description: String
  def version: String
  def cli: CliOpt[T]
  def run: T => Unit
}
