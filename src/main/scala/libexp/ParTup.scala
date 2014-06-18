/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package libexp

/**
 * Created by thomas on 17.06.14.
 */
case class ParTup[A,U](values: Iterable[(A,U)], reporter: Report[(A,U)]) extends Exp[A]{
  override type TRet = ParTup[A,U]
  override type TMap[B] = ParTup[B,(A,U)]
  override type TFMap[X,Y] = PTFMap[A,U,X,Y]

  override def run: Iterable[Seq[String]] = values.map(reporter.apply)
  override def names: Seq[String] = reporter.names

  override def addReport(r: (String, PartialFunction[A, String])): TRet =
    ParTup(values,reporter + (r._1 -> Report.composePF(PartialFunction[(A,U),A](_._1), r._2)))

  override def map[B](f: (A) => B): TMap[B] =
    ParTup(values.view.map{case (a,u) => (f(a),(a,u))}, reporter.comap(PartialFunction((_: (B,(A,U)))._2)))

  override def flatMap[B,R](f: A => B)(implicit ev: TFMap[B,R]): R = ev.flatMap(this,f)
}

trait PTFMap[A,U,B,R]{
  def flatMap(pt: ParTup[A,U], f: A => B): R
}
object PTFMap {
  //maybe simpler implementation when returning ParTup[B,((B,BU),(A,AU))]
  implicit def ptfmap[A,U,B, BU, R] = new PTFMap[A,U,ParTup[B, BU], ParTup[B, (BU, (A, U))]] {
    override def flatMap(pt: ParTup[A,U], f: (A) => ParTup[B, BU]): ParTup[B, (BU, (A, U))] = {
      val unflattened: Iterable[(ParTup[B, BU], (A, U))] = pt.values.view.map { case t@(a, _) => (f(a), t)}
      val bRep: Report[(B, BU)] = unflattened.head._1.reporter
      val newValues = for {
        (pt, au) <- unflattened
        (b, bu) <- pt.values.view
      } yield (b, (bu, au))
      val newReporter = pt.reporter.comap(PartialFunction[(B, (BU, (A, U))), (A, U)] { case (b, (bu, au)) => au}) ++
        bRep.comap(PartialFunction[(B, (BU, (A, U))), (B, BU)] { case (b, (bu, _)) => (b, bu)})
      ParTup(newValues, newReporter)
    }
  }
}