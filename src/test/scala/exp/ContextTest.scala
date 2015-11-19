package exp

import org.specs2.mutable.Specification
import scalaz.syntax.apply._

class ContextTest extends Specification {

  "collecting columns must work" >> {
    val f1 = Fixed[Int]("number.1", 1 to 3)
      .addColumn("n1", _.toString)
    val f2 = Fixed[Int]("number.2", 1 to 3)
      .addColumn("n2", _.toString)

    val result = ^(f1,f2)(_ * _).addColumn("product", _.toString)

    result.columns.map(_._2).toSet === Set("n1","n2","product")
  }
}
