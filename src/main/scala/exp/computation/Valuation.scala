package exp.computation

/**
  * Created by thomas on 02.12.15.
  */
case class Valuation(assignment: Map[CNode,Any]){
  def apply(n: CNode): Any = assignment(n)
  def +(entry: (CNode,Any)): Valuation = Valuation(assignment + entry)

  override def toString: String = s"Val(${assignment.map{case (k,v) => s"${k.name}:$v"}.mkString(";")})"
}
