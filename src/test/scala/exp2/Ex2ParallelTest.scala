package exp2

import org.specs2._
import org.specs2.specification.Fragments
import org.specs2.matcher.{MatchResult, Expectable, Matcher}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 8/30/13
 */
class Ex2ParallelTest extends Specification with Ex2Matchers {

  def beOrderedBy[A,S: Ordering,G](ordered: A => S, group: A => G = (_: A) => 0): Matcher[Seq[A]] = new Matcher[Seq[A]]{
    def findUnorderedElement(s: Seq[A]): Option[(A,A)] = {
      val grouped = s.groupBy(group)
      def findUnordered(seq: Seq[A]): Option[(A,A)] = seq.zip(seq.tail).find{ case (a,b) =>
        implicitly[Ordering[S]].compare(ordered(a),ordered(b)) > 0
      }
      grouped.values.map(findUnordered).find(_.isDefined).flatten
    }

    def apply[S <: Seq[A]](t: Expectable[S]): MatchResult[S] = {
      val proof: Option[(A, A)] = findUnorderedElement(t.value)
      result(!proof.isDefined,"everything ordered properly", f"element ${proof.get._1} is ordered before ${proof.get._2}",t)
    }
  }

  def parR(n: Int = 100): Ex2[Int, Int] = Ex2(1 to n)
  def seqR(n: Int = 100): Ex2[Int, Int] = {
    val container = Array(0)
    val eval: () => Int = {
      () =>
        container(0) = container(0) + 1
        container(0)
    }
    val seqEx: Ex2[Int, Int] = Ex2.sequential(() => Iterator.continually(eval()).take(n))
    seqEx
  }
  def parR100 = parR(100)
  def seqR100 = seqR(100)

  def timeMap: Any => Long = _ => System.nanoTime

  def is: Fragments =
  "serial experiment must be serial" ! (seqR100 must haveEqualTrials(1 to 100)) ^
  "serial experiment must be serial after mapping" !
    (seqR100.map(_ + 1) must haveEqualTrials(1 to 100 map (x => (x + 1,x)))) ^
  "result of serial experiment after mapping must be correct" !
    (seqR100.map(_ + 1).logAs("i") must haveSameResultAs(Ex2.values(2 to 101:_*).logAs("i"))) ^
  "mixing of serial and parallel experiments" ^
    "test matcher" ! (Seq(2,1) must beOrderedBy(identity)).not ^
    "test matcher 2" ! (Seq(("a",1),("b",3),("a",2),("b",4)) must beOrderedBy(_._2,group = _._1)) ^
    "test matcher 3" ! (Seq(("a",1),("b",3),("a",2),("b",4),("b",1)) must beOrderedBy(_._2,group = _._1)).not ^
    "test size" ! ((parR100 zip seqR100 zip seqR100).iterable.size === 1000000) ^
    "retention of order of sequential part" ^
      "(par,seq)" !
        (seqR(20).map{_ => Thread.sleep((math.random * 100).toInt); System.nanoTime()}.logAs("t").run.map(_.head).toIndexedSeq must beOrderedBy(identity)) ^
    p^
    "if this fails, there is no multi-core support to empirically verify correctness" !
      (parR(20).map{_ => Thread.sleep((math.random * 100).toInt); System.nanoTime()}.logAs("t").run.map(_.head).toIndexedSeq must beOrderedBy(identity)).not
}
