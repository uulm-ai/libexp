package exp3


case class TypedComputation[R,NA,F](name: String,
                                    deps: NA,
                                    computation: F,
                                    pl: ParList.Aux[_,R,NA,F],
                                    override val columns: Seq[(String, R => String)]) extends UntypedComputation[R]{
  override def predecessors: Seq[Node] = pl.listDeps(deps)
  override def compute(args: Seq[Any]): R = pl.listFunction(computation)(args)
  def report(colName: String, f: R => String) = copy(columns = columns :+ (colName,f))
}

object TypedComputation{
  def apply[DN,F,R](name: String, dependencies: DN)(computation: F)(implicit pl: ParList.Aux[_,R,DN,F]): TypedComputation[R,DN,F] =
    new TypedComputation(name,dependencies,computation,pl,Seq())
}
