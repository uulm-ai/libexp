package exp.node

import scalaz.~>

/**
  * Created by thomas on 26.11.15.
  */
case object RngInsertion extends Stage {
  override type Payload[+T] = Unit
  type This = RngInsertion.type

  override type Next = Base.type
  override def nextStage: RngInsertion.Next = Base

  def seed(name: String): Inject[RngInsertion.type, Long] = Inject[RngInsertion.type, Long](Unit, Some(name))

  def insertSeeds[T](seeds: Seq[Long], node: StageNode[T]): NextNode[T] = {
    import syntax._
    val baseSeed: Node[Base.type,Long] = Base.fromSeq(seeds, "seed.base")
      .addColumn("seed.base")

    val allSeedNodes: Seq[InjectNode[Any]] = node.allNodesOnStage.collect{
      case i: Inject[This,_] => i
    }.distinct.sortBy(_.name)

    val procInjects: InjectNode ~> NextNode = new ~>[InjectNode,NextNode]{
      override def apply[A](fa: InjectNode[A]): NextNode[A] = {
        import syntax._
        val idx = allSeedNodes.indexOf(fa)
        baseSeed.map(_ + idx, effort = Effort.none, name = s"seed.${fa.name.get}").asInstanceOf[NextNode[A]]
      }
    }
    runStage(procInjects)(node)
  }
}
