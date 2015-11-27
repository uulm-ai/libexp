package exp.node

/**
  * Created by thomas on 26.11.15.
  */
case object RngInsertion extends Stage {
  override type Payload[+T] = Unit

  def seed(name: String): Inject[RngInsertion.type, Long] = Inject[RngInsertion.type, Long](this, Unit, Some(name))

  implicit def aboveBase: StageAfter[Base.type, RngInsertion.type] = StageAfter(this)

  def insertSeeds[S <: Stage, T](seeds: Seq[Long], node: Node[S, T])(implicit stageCast: StageCast[S, RngInsertion.type]): Node[Base.type, T] ={
    import syntax._
    val baseSeed: Node[Base.type,Long] = Base.fromSeq(seeds, "seed.base").addColumn("seed.base")
    val allSeedNodes = node.allNodes.collect{
      case i@Inject(RngInsertion, (), Some(name)) => i
    }.toSeq.sortBy(_.name)

    stageCast(node) match {
      case i@Inject(RngInsertion, (), Some(name)) =>
        import syntax._
        val idx = allSeedNodes.indexOf(i)
        baseSeed.map(_ + idx, effort = Effort.none, name = s"seed.$name").asInstanceOf[BaseNode[T]]
      case Wrap(RngInsertion, n) => n.asInstanceOf[Node[Base.type, T]]
      case App(RngInsertion, ins, f, e, n) => App(Base, ins.map(insertSeeds(seeds, _)), f, e, n)
      case Lift(RngInsertion, p, pie, el, n) => Lift(Base, insertSeeds(seeds, p), pie, el, n)
      case Report(RngInsertion, n, cn, f) => Report(Base, insertSeeds(seeds, n), cn, f.asInstanceOf[T => String])
      case otherwise => sys.error(s"inexhaustive match in RngInsertion on node $otherwise")
    }
  }
}
