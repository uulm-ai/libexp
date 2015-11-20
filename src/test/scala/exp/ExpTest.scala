package exp

import org.specs2.mutable.Specification

/**
  * Created by thomas on 20.11.15.
  */
class ExpTest extends Specification {
  "fixed experiment" >> {
    val f1 = fromSeq(1 to 2).withReport("f1", _.toString)
    val f2 = fromSeq(1 to 2).withReport("f2", _.toString)
    1 === 2
  }

}
