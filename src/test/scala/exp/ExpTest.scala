/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package exp

import org.specs2.Specification
import java.lang.management.ManagementFactory

class ExpTest extends Specification {
  def is =
    "equality" ^
      (Exp.values() === Exp.values()) ^
      (Exp.values(1) === Exp.values(1)) ^
      (Exp.values(1,2) === Exp.values(1,2)) ^
    p^
    "empty experiment must run and have empty header" ! (Exp().create._1 === Seq()) ^
    "empty experiment must have single empty row" ! (Exp().create._2.toList === List(List())) ^
    (Exp().flatMap(_ => Exp.values(1,2).addColumn("n",_.toString)).create._2.toSeq === List(List("1"),List("2"))) ^
    "expressions may only be evaluated once" ! {
      var i = 0
      val iterable = Iterator.continually({i += 1; i}).take(2)

      (for {
        i <- Exp.fromIterator(iterable).addColumn("n",_.toString)
      } yield Unit).create._2.toIndexedSeq === Seq(Seq("1"),Seq("2"))
    } ^
    "test time" ! {
      val exp = for{
        trial <- Exp.seed(1,3)
        value = {
          waitFor(trial.toDouble)
          trial
        }
      } yield ()
      println(exp.reportCpuTimeAs("time").toString)
      true
    }

  def waitFor(s: Double): Unit = {
    val now = ManagementFactory.getThreadMXBean.getCurrentThreadCpuTime
    while(ManagementFactory.getThreadMXBean.getCurrentThreadCpuTime - now < s * 1e9){}
  }
}