package exp.node

import exp.cli.{CliOpt, CLI}

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
  def fromCli[S <: Stage,T](cli: CliOpt[Node[S,T]])(implicit cast: StageCast[S,RngInsertion.type]): CliNode[T] =
    CliProc.cliOpt(cli)(cast)

  def ^[S1 <: Stage, S2 <: Stage, T1, T2, R](n1: Node[S1,T1], n2: Node[S2,T2], effort: Effort = Effort.low,name: String = "")(f: (T1,T2) => R)(implicit lub: StageLUB[S1,S2]): Node[lub.Out,R] =
    App.map2(n1,n2,effort,Some(name).filterNot(_ == ""))(f)(lub)

  implicit class RichNode[St <: Stage, T](n: Node[St,T]) {
    //mapping and lifting
    def map[S](f: T => S, effort: Effort = Effort.low, name: String = ""): Node[St,S] = App.map(n,effort, Some(name).filterNot(_ == ""))(f)
    def lift[S](estimatedLength: Double = 10, name: String = "")(implicit ev: T <:< Stream[S]): Node[St,S] =
      Lift(n.asInstanceOf[Node[St,Stream[S]]], Effort.none, expectedLength = Length(estimatedLength), name = Some(name).filterNot(_ == ""))
    //annotation and reporting
    def addColumn(name: String, f: T => String = _.toString): Report[St, T, T] = Report(n, name, f)
    def *>[S2 <: Stage](ignored: Node[S2,_])(implicit lub: StageLUB[St,S2]): Node[lub.Out,T] = App.ignoreRight(n,ignored)(lub)
  }
}
