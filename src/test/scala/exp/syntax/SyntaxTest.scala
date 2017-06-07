package exp.syntax

import java.io.{ByteArrayOutputStream, PrintStream}

import org.specs2.mutable.Specification
import exp.node.syntax._
import exp.predefs._

class SyntaxTest extends Specification {

  "whole experiment" >> {
    val inInt = intInput("foo", "")
    val inInt2 = intInput("foo2", "")
    val result = ^(inInt,inInt2)(_ * _).addColumn("r",_.toString)
    val buffer = new ByteArrayOutputStream(1000)
    exp.application.runStandaloneExperiment(result, "foo", Array("--foo","2","--foo2","3"), out = new PrintStream(buffer))
    buffer.close()
    buffer.toString.lines.filterNot(_.head == '#').drop(1).next().split("\t").map(_.toInt).toSet === Set(2,3,6)
   }
}
