package exp.computation

/**
  * Created by thomas on 02.12.15.
  */
case class Valuation(assignment: Map[CEdge,Any]){
  def apply(n: CEdge): Any = assignment(n)
  def +(entry: (CEdge,Any)): Valuation = Valuation(assignment + entry)

  override def toString: String = s"Val(${assignment.map{case (k,v) => s"${k.name}:$v"}.mkString(";")})"
}
