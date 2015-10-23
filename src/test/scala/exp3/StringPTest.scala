package exp3

import fastparse.all._
import fastparse.core.Result.Success
import org.specs2.mutable.Specification

/**
 * Created by thomas on 07.10.15.
 */
class StringPTest extends Specification {

  "string parser should" >> {
    P(CharsWhile(c => !Set(',')(c),min = 1).!.rep(sep=",")).parse("abc,def") must beAnInstanceOf[Success[_]]
  }
}
