package exp2

import org.specs2.matcher._

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 8/30/13
 */
trait Ex2Matchers {
  import TraversableMatchers._
  def haveEqualTrials[U](other: Seq[U]): Matcher[Ex2[_,U]] =
    containTheSameElementsAs(other.toIndexedSeq) ^^ ((_:Ex2[_,U]).iterable.toIndexedSeq)

  def haveEqualTrialsAs[U](other: Ex2[_,U]): Matcher[Ex2[_,U]] = haveEqualTrials(other.iterable.toIndexedSeq)

  def haveOrderedEqualTrials[U](trials: Seq[U]): Matcher[Ex2[_,U]] = new BeEqualTo(trials.toIndexedSeq) ^^ ((_:Ex2[_,U]).iterable.toIndexedSeq)

  def haveSameResultAs(other: Ex2[_,_]): Matcher[Ex2[_,_]] =
    new BeEqualTo(other.run.toBuffer) ^^ ((_:Ex2[_,_]).run.toBuffer) and new BeEqualTo(other.colNames) ^^ ((_:Ex2[_,_]).colNames)
}
