package exp.computation

import cats.Monoid

/**
  * Created by thomas on 02.12.15.
  */
case class Valuation(assignment: Map[CNode,Any]) extends (CNode => Any) {
  def apply(n: CNode): Any = assignment(n)
  def +(entry: (CNode,Any)): Valuation = Valuation(assignment + entry)

  override def toString: String = s"Val(${assignment.map{case (k,v) => s"${k.name}:$v"}.mkString(";")})"
}

object Valuation{
  def empty: Valuation = Valuation(Map())
  implicit val monoidInst: Monoid[Valuation] = new Monoid[Valuation]{
    override def empty: Valuation = Valuation.empty
    override def combine(x: Valuation, y: Valuation): Valuation = Valuation(x.assignment ++ y.assignment)
  }
}
