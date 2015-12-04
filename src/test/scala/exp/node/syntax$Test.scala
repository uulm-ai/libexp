package exp.node

import org.specs2.mutable.Specification

/**
  * Created by thomas on 25.11.15.
  */
class syntax$Test extends Specification {

  import syntax._


  "compile-time: StageLess instances" >> {
    implicitly[StageLess[Base.type,RngInsertion.type]]
    implicitly[StageLess[RngInsertion.type,CliProc.type]]
    1 === 1
  }

  "compile-time: stage cast instances" >> {
    "StageCast equal" >> {
      implicitly[StageCast[Base.type,Base.type]](StageCast.castEqual)
      implicitly[StageCast[RngInsertion.type ,RngInsertion.type]]
      implicitly[StageCast[CliProc.type,CliProc.type]]
      1 === 1
    }
    "one step" >> {
      implicitly[StageCast[Base.type, RngInsertion.type]]
      implicitly[StageCast[RngInsertion.type, CliProc.type]]
      1 === 1
    }
    "transitive" >> {
      implicitly[StageCast[Base.type, CliProc.type]]
      1 === 1
    }
  }

  "find StageLUB instances" >> {
    implicitly[StageLUB.Aux[Base.type, Base.type, Base.type]]

    implicitly[StageLUB.Aux[Base.type, RngInsertion.type, RngInsertion.type]]
    implicitly[StageLUB.Aux[RngInsertion.type, Base.type, RngInsertion.type]]

    implicitly[StageLUB.Aux[Base.type, CliProc.type, CliProc.type]]
    implicitly[StageLUB.Aux[CliProc.type, Base.type, CliProc.type]]
    1 === 1
  }

  "lifting" >> {
    val f: BaseNode[Int] = fromSeq(1 to 5, "length")
      .map(n => Stream.from(1).take(n)).lift(1)

    success
  }

  "app syntax" >> {
    val f: BaseNode[Int] = ^(fromSeq(1 to 5, "f"),fromSeq(1 to 5, "g"))(_ + _)
    success
  }

}
