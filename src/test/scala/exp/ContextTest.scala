package exp

import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import scalaz.syntax.apply._

class ContextTest extends Specification {

  "collecting columns must work" >> {
    val f1 = Fixed[Int]("n1", 1 to 3).reportVariable
    val f2 = Fixed[Int]("n2", 1 to 3).reportVariable

    val result = ^(f1,f2)(_ * _).addColumn("product", _.toString)

    result must haveColumns("n1","n2","product")
  }

//  "context works with compuation" >> {
//    val f1 = Fixed("n1", 1 to 3).reportVariable
//    val f2 = Fixed("n2", 7 to 9).reportVariable
//
//    val computed = Computation("prod",0.001d)((f1,f2))((_: Int) * (_: Int))
//      .reportVariable
//
//    computed must haveColumns("n1","n2","product")
//  }

  def haveColumns(cols: String*): Matcher[Context[_]] =
    containTheSameElementsAs(cols.toSeq) ^^ ((_:Context[_]).columns.map(_._2))
}
