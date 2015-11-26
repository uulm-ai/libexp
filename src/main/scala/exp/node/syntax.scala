package exp.node

import exp.cli.CliOpt

/**
  * Created by thomas on 26.11.15.
  */
object syntax {
  type BaseNode[+T] = Node[Base.type,T]
  type RngNode[+T]  = Node[RngInsertion.type,T]
  type CliNode[+T]  = Node[CliProc.type,T]
  type N[+T] = CliNode[T]

  implicit def convertStage[From <: Stage,To <: Stage, T](n: Node[From,T])(implicit sc: StageCast[From,To]): Node[To,T] = sc(n)

  //node constructors
  def fromSeq[T](xs: Seq[T], name: String): BaseNode[T] = Base.fromSeq(xs.toIndexedSeq,name)
  def seed(n: String): RngNode[Long] = RngInsertion.seed(n)
  def fromCli[T](cli: CliOpt[RngNode[T]]): CliNode[T] = CliProc.cliOpt(cli)

  def ^[S1 <: Stage, S2 <: Stage, T1, T2, R](n1: Node[S1,T1], n2: Node[S2,T2], effort: Effort = Effort.low)(f: (T1,T2) => R)(implicit lub: StageLUB[S1,S2]): Node[lub.Out,R] =
    App.map2(n1,n2,effort)(f)(lub)
  implicit class RichNode[St <: Stage, T](n: Node[St,T]) {
    //mapping and lifting
    def map[S](f: T => S, effort: Effort = Effort.low): Node[St,S] = App.map(n,effort)(f)
    def lift[S](implicit ev: T <:< Stream[S]): Node[St,S] = Lift(n.stage,n.asInstanceOf[Node[St,Stream[S]]])
    //annotation and reporting
    def addColumn(name: String, f: T => String): Report[St, T, T] = Report(n.stage,n, name, f)
  }
}
