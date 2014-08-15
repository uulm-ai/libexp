/*
 * Copyright (c) year Thomas Geier. This source code may be used under the MIT license.
 */

package libexp

import org.specs2.Specification
import org.specs2.specification.Fragments

class ParTupTest extends Specification {
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
