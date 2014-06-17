/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package libexp

import org.specs2.Specification
import org.specs2.specification.Fragments

/**
 * Created by thomas on 17.06.14.
 */
class ParTupTest extends Specification {
  import Exp._

  override def is: Fragments =
    "simple experiment" ! {
      val e = for{
        i <- seq(1 to 2).report("i")
        j <- seq(1 to 2).report("j")
      } yield i * j
      e.report("i*j").printCSV()
      true
    }
}