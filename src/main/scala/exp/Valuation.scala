package exp

/**
  * Created by thomas on 17.11.15.
  */
case class Valuation(map: Map[String,Any]) {
  def apply[T](node: Node[T]): T = map(node.name).asInstanceOf[T]
  def +[T](kv: (Node[T], T)): Valuation = Valuation(map.updated(kv._1.name, kv._2))
}
