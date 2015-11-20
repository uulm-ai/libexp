package exp

import exp.stages.cli.CliOpt
import org.specs2.mutable.Specification
import fastparse.all._

class CliOpt$Test extends Specification with ValMatchers {
  import CliOpt._

  val someReader: Reader[String] = parserToReader(P("abc").!)

  "having multiple inputs with same name must fail" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short = Some('f')), CliOpt[String]("bar", someReader, short = Some('f')))
    CliOpt.parse(Array(),cliOpts) must beFailure
  }

  "not providing required argument must fail" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short = Some('f')))
    CliOpt.parse(Array(),cliOpts) must beFailure
  }

  "not being able to parse data completely must be failure" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short= Some('f')))
    CliOpt.parse(Array("--foo", "abcd"),cliOpts) must beFailure
  }

  "succeed parsing single required option with long arg" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short= Some('f')))
    CliOpt.parse(Array("--foo", "abc"),cliOpts) must beSuccess
  }

  "succeed parsing single required option with short arg" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short= Some('f')))
    CliOpt.parse(Array("-f", "abc"),cliOpts) must beSuccess
  }

  "fail giving unknown arg" >> {
    val cliOpts = Seq(CliOpt[String]("foo", someReader, short= Some('f')))
    CliOpt.parse(Array("-g", "abc"),cliOpts) must beFailure
  }

}

