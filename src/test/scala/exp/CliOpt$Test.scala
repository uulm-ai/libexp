package exp

import org.specs2.mutable.Specification
import fastparse.all._

class CliOpt$Test extends Specification with ValMatchers {

  "having multiple inputs with same name must fail" >> {
    val cliOpts = Seq(CliOpt[String]("foo", Some('f'), P("abc").!), CliOpt[String]("bar", Some('f'), P("abc").!))
    CliOpt.parse(Array(),cliOpts) must beFailure
  }

  "not providing required argument must fail" >> {
    val cliOpts = Seq(CliOpt[String]("foo", Some('f'), P("abc").!))
    CliOpt.parse(Array(),cliOpts) must beFailure
  }

  "not being able to parse data must be failure" >> {
    val cliOpts = Seq(CliOpt[String]("foo", Some('f'), P("abc").!))
    CliOpt.parse(Array("--foo", "abcd"),cliOpts) must beFailure
  }

  "succeed parsing single required option with long arg" >> {
    val cliOpts = Seq(CliOpt[String]("foo", Some('f'), P("abc").!))
    CliOpt.parse(Array("--foo", "abc"),cliOpts) must beSuccess
  }

  "succeed parsing single required option with short arg" >> {
    val cliOpts = Seq(CliOpt[String]("foo", Some('f'), P("abc").!))
    CliOpt.parse(Array("-f", "abc"),cliOpts) must beSuccess
  }

  "fail giving unknown arg" >> {
    val cliOpts = Seq(CliOpt[String]("foo", Some('f'), P("abc").!))
    CliOpt.parse(Array("-g", "abc"),cliOpts) must beFailure
  }

}

