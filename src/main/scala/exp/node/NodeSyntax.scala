package exp.node

import exp.parsers
import exp.cli.Read
import fastparse.all._

import scala.language.higherKinds


final case class Table(name: String)
object Table{
  final val default:  Table = Table("default")
}

/** Syntax for nodes. */
trait NodeSyntax { outer =>
  type N[+T]

  def name(n: N[Any]): String
  def pure[T](x: T, name: String): N[T]
  def lift[T,S](n: N[T],estimatedLength: Double = 10, name: String = "")(implicit ev: T <:< Iterable[S]): N[S]

  def addColumn[T](n: N[T], name: String, f: T => String, tables: Set[Table] = Set(Table.default)): N[T]

  def mAppNUntyped[R](nodes: IndexedSeq[N[Any]],
                      f: IndexedSeq[Any] => R,
                      name: String = "", effort: Effort = Effort.low): N[R]
  def cli[T](name: String, parser: Read[T], description: String, format: String = "", default: Option[T] = None): N[T]

  def ignore[T](taken: N[T], ignored: N[Any]): N[T]

  // derived functions below here

  protected[exp] val baseSeed: N[Long] = lift(cli[Seq[Long]](
    "seeds",
    Read.fromParser(P(parsers.pPosLong ~ ":" ~ parsers.pPosLong).map(x => x._1 to x._2)),
    "the set of RNG seeds to use",
    "n:m for the range `n` to `m`",
    Some(Seq(1))
  ), 100, "base.seed").addColumn("base.seed", _.toString)

  def seed(name: String): N[Long] = baseSeed.map(s => (s,name).hashCode, name)
  def cliSeq[T](name: String,
                parser: exp.cli.Read[Seq[T]],
                description: String,
                format: String,
                default: Option[Seq[T]]): N[T] = cli(name,parser,description,format,default).lift(1,s"$name.lifted")
  def fromSeq[T](xs: Seq[T], name: String): N[T] = lift(pure(xs, name), xs.size.toDouble, s"$name.lifted")

  def ^[T1,T2,R](n1: N[T1], n2: N[T2], name: String = "", effort: Effort = Effort.low)(f: (T1,T2) => R): N[R] =
    mAppNUntyped(IndexedSeq(n1,n2), ins => f(ins(0).asInstanceOf[T1],ins(1).asInstanceOf[T2]), Some(name).filter(_.nonEmpty).getOrElse(s"^(${n1.name},${n2.name})"), effort)

  def ^^[T1,T2,T3,R](n1: N[T1], n2: N[T2], n3: N[T3], name: String = "", effort: Effort = Effort.low)(f: (T1,T2,T3) => R): N[R] =
    mAppNUntyped(IndexedSeq(n1,n2,n3), ins => f(ins(0).asInstanceOf[T1],ins(1).asInstanceOf[T2], ins(2).asInstanceOf[T3]), Some(name).filter(_.nonEmpty).getOrElse(s"^^(${n1.name},${n2.name},${n3.name})"), effort)

  def ^^^[T1,T2,T3,T4,R](n1: N[T1], n2: N[T2], n3: N[T3], n4: N[T4], name: String = "", effort: Effort = Effort.low)(f: (T1,T2,T3,T4) => R): N[R] =
    mAppNUntyped(
      IndexedSeq(n1,n2,n3,n4),
      ins => f(ins(0).asInstanceOf[T1],ins(1).asInstanceOf[T2], ins(2).asInstanceOf[T3], ins(3).asInstanceOf[T4]),
      Some(name).filter(_.nonEmpty).getOrElse(s"^^(${n1.name},${n2.name},${n3.name},${n4.name})"),
      effort)

  def ^^^^[T1,T2,T3,T4,T5,R](n1: N[T1], n2: N[T2], n3: N[T3], n4: N[T4], n5: N[T5], name: String = "", effort: Effort = Effort.low)(f: (T1,T2,T3,T4,T5) => R): N[R] =
    mAppNUntyped(
      IndexedSeq(n1,n2,n3,n4,n5),
      ins => f(ins(0).asInstanceOf[T1],ins(1).asInstanceOf[T2], ins(2).asInstanceOf[T3], ins(3).asInstanceOf[T4], ins(4).asInstanceOf[T5]),
      Some(name).filter(_.nonEmpty).getOrElse(s"^^(${n1.name},${n2.name},${n3.name},${n4.name},${n5.name})"),
      effort)

  def ^^^^^[T1,T2,T3,T4,T5,T6,R](n1: N[T1], n2: N[T2], n3: N[T3], n4: N[T4], n5: N[T5], n6: N[T6], name: String = "", effort: Effort = Effort.low)(f: (T1,T2,T3,T4,T5,T6) => R): N[R] =
    mAppNUntyped(
      IndexedSeq(n1,n2,n3,n4,n5,n6),
      ins => f(ins(0).asInstanceOf[T1],ins(1).asInstanceOf[T2], ins(2).asInstanceOf[T3], ins(3).asInstanceOf[T4], ins(4).asInstanceOf[T5], ins(5).asInstanceOf[T6]),
      Some(name).filter(_.nonEmpty).getOrElse(s"^^(${n1.name},${n2.name},${n3.name},${n4.name},${n5.name},${n6.name})"),
      effort)

  implicit class RichNode[T](val n: N[T]) {
    def name: String = outer.name(n)
    //mapping and lifting
    def map[S](f: T => S, name: String = s"map.${n.name}", effort: Effort = Effort.low): N[S] =
      outer.mAppNUntyped(IndexedSeq(n),ins => f(ins(0).asInstanceOf[T]), name, effort)
    def lift[S](estimatedLength: Double = 10, name: String = s"lift.${n.name}")(implicit ev: T <:< Iterable[S]): N[S] =
      outer.lift(n, estimatedLength, name)
    //annotation and reporting
    def addColumn(name: String, f: T => String = _.toString, tables: Set[Table] = Set(Table.default)): N[T] =
      outer.addColumn(n, name, f, tables)
    def <*(ignored: N[Any]): N[T] = ignore(n, ignored)
    def *>[TT](taken: N[TT]): N[TT] = ignore(taken,n)
  }
}