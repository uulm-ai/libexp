package exp

import scalaz.Apply

///** A Context holds a node, and also the columns producing the observed variables. */
//case class Context[+T](node: Node[T], columns: Seq[(String, String, Any => String)] = Seq()){
//  def addColumn(name: String, report: T => String): Context[T] =
//    this.copy(columns = columns :+ (node.name, name, ((_: Any).asInstanceOf[T]) andThen report))
//  def reportVariable: Context[T] = addColumn(node.name, _.toString)
//  def reportVariable(f: T => String): Context[T] = addColumn(node.name, f)
//}
//
//object Context {
//  import scalaz.syntax.apply._
//  implicit def appInstance[T]: Apply[Context] = new Apply[Context]{
//    override def ap[A, B](fa: => Context[A])(f: => Context[(A) => B]): Context[B] =
//      Context(^(fa.node,f.node)((a,g) => g(a)), fa.columns ++ f.columns)
//
//    override def map[A, B](fa: Context[A])(f: (A) => B): Context[B] =
//      Context(fa.node.map(f), fa.columns)
//  }
//}
