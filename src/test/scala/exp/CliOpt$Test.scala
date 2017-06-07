package exp

import exp.cli._
import org.specs2.mutable.Specification
import fastparse.all._

class CliOpt$Test extends Specification {

  val someReader: Read[String] = Read.fromParser(P("abc").!)

  "having multiple inputs with same name must fail" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short = Some('f')), CliOpt[String]("bar", someReader, short = Some('f')))
    CliOpt.parse(Array(),cliOpts) must beLeft
  }

  "not providing required argument must fail" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short = Some('f')))
    CliOpt.parse(Array(),cliOpts) must beLeft
  }

  "not being able to parse data completely must be failure" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short= Some('f')))
    CliOpt.parse(Array("--foo", "abcd"),cliOpts) must beLeft
  }

  "succeed parsing single required option with long arg" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short= Some('f')))
    CliOpt.parse(Array("--foo", "abc"),cliOpts) must beRight
  }

  "succeed parsing single required option with short arg" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short= Some('f')))
    CliOpt.parse(Array("-f", "abc"),cliOpts) must beRight
  }

  "fail giving unknown arg" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short= Some('f')))
    CliOpt.parse(Array("-g", "abc"),cliOpts) must beLeft
  }

}

