package exp.node

import exp.cli.CliOpt
import shapeless._
import fastparse.all._

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
  def fromCliSeq[T](name: String, description: String, format: String, parser: P[Seq[T]], default: Option[Seq[T]]): CliNode[T] =
    fromCli(CliOpt(
      long = name,
      valueParser = exp.cli.Read.fromParser(parser.map(fromSeq(_,name))),
      description = description,
      short = None,
      default = default.map(fromSeq(_,name)),
      formatDescription = format))

  def ^[S1 <: Stage, S2 <: Stage, T1, T2, R](n1: Node[S1,T1], n2: Node[S2,T2], effort: Effort = Effort.low,name: String = "")(f: (T1,T2) => R)(implicit lub: StageLUB[S1,S2]): Node[lub.Out,R] =
    App.map2(n1,n2,effort,Some(name).filterNot(_ == ""))(f)(lub)

  def ^^[N1,N2,N3,T1,T2,T3,R](n1: N1, n2: N2, n3: N3, effort: Effort = Effort.low,name: String = "")(f: (T1,T2,T3) => R)(implicit sl: StageList.Aux2[N1 :: N2 :: N3 :: HNil,T1 :: T2 :: T3 :: HNil]): Node[sl.S,R] =
    App(sl.castNodes(n1 :: n2 :: n3 :: HNil), { ins =>
      val t1 :: t2 :: t3 :: HNil = sl.castFun(ins)
      f(t1,t2,t3)
    }, effort, Some(name).filterNot(_.isEmpty))

  def ^^^[N1,N2,N3,N4,T1,T2,T3,T4,R](n1: N1, n2: N2, n3: N3, n4: N4, effort: Effort = Effort.low,name: String = "")(f: (T1,T2,T3,T4) => R)(implicit sl: StageList.Aux2[N1 :: N2 :: N3 :: N4 :: HNil,T1 :: T2 :: T3 :: T4 :: HNil]): Node[sl.S,R] =
    App(sl.castNodes(n1 :: n2 :: n3 :: n4 :: HNil), { ins =>
      val t1 :: t2 :: t3 :: t4 :: HNil = sl.castFun(ins)
      f(t1,t2,t3, t4)
    }, effort, Some(name).filterNot(_.isEmpty))

  def ^^^^[N1,N2,N3,N4,N5,T1,T2,T3,T4,T5,R](n1: N1, n2: N2, n3: N3, n4: N4, n5: N5, effort: Effort = Effort.low,name: String = "")(f: (T1,T2,T3,T4,T5) => R)(implicit sl: StageList.Aux2[N1 :: N2 :: N3 :: N4 :: N5 :: HNil,T1 :: T2 :: T3 :: T4 :: T5 :: HNil]): Node[sl.S,R] =
    App(sl.castNodes(n1 :: n2 :: n3 :: n4 :: n5 :: HNil), { ins =>
      val t1 :: t2 :: t3 :: t4 :: t5 :: HNil = sl.castFun(ins)
      f(t1,t2,t3, t4, t5)
    }, effort, Some(name).filterNot(_.isEmpty))

  implicit class RichNode[St <: Stage, T](n: Node[St,T]) {
    //mapping and lifting
    def map[S](f: T => S, effort: Effort = Effort.low, name: String = ""): Node[St,S] = App.map(n,effort, Some(name).filterNot(_ == ""))(f)
    def lift[S](estimatedLength: Double = 10, name: String = "")(implicit ev: T <:< Stream[S]): Node[St,S] =
      Lift(n.asInstanceOf[Node[St,Stream[S]]], Effort.none, expectedLength = Length(estimatedLength), name = Some(name).filterNot(_ == ""))
    //annotation and reporting
    def addColumn(name: String, f: T => String = _.toString): Report[St, T, T] = Report(n, name, f)
    def *>[S2 <: Stage,TT](taken: Node[S2,TT])(implicit lub: StageLUB[S2,St]): Node[lub.Out,TT] = App.ignoreRight(taken,n)(lub)
    def <*[S2 <: Stage](ignored: Node[S2,_])(implicit lub: StageLUB[St,S2]): Node[lub.Out,T] = App.ignoreRight(n,ignored)(lub)
  }
}
