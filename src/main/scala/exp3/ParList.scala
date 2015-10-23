package exp3

object ParList{
  type Aux[A,R,-Na,Fa] = ParList[A,R]{
    type NA >: Na
    type F = Fa
  }
  implicit def t1[A,R]: ParList.Aux[A, R, ValuedNode[A], A => R] = new ParList[A,R]{
    override type NA = ValuedNode[A]
    override type F = A => R
    override def listDeps(deps: ValuedNode[A]): Seq[Node] = Seq(deps)
    override def listFunction(f: A => R): (Seq[Any]) => R = { xs =>
      f(xs.head.asInstanceOf[A])
    }
  }
  implicit def t2[X1,X2,R]: ParList.Aux[(X1, X2), R,(ValuedNode[X1],ValuedNode[X2]), (X1,X2) => R]  =
    new ParList[(X1,X2),R]{
      override type NA = (ValuedNode[X1],ValuedNode[X2])
      override type F = (X1,X2) => R
      override def listDeps(deps: (ValuedNode[X1], ValuedNode[X2])): Seq[Node] = Seq(deps._1,deps._2)
      override def listFunction(f: (X1, X2) => R): (Seq[Any]) => R = { xs =>
        val x1 = xs(0).asInstanceOf[X1]
        val x2 = xs(1).asInstanceOf[X2]
        f(x1,x2)
      }
    }
  implicit def t3[X1,X2,X3,R]: ParList.Aux[(X1,X2,X3), R, (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3]), (X1, X2, X3) => R]  =
    new ParList[(X1,X2,X3), R]{
      override type NA = (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3])
      override type F = (X1, X2, X3) => R
      override def listDeps(deps: (ValuedNode[X1], ValuedNode[X2], ValuedNode[X3])): Seq[Node] = Seq(deps._1,deps._2,deps._3)
      override def listFunction(f: (X1, X2, X3) => R): (Seq[Any]) => R = { xs =>
        val x1 = xs(0).asInstanceOf[X1]
        val x2 = xs(1).asInstanceOf[X2]
        val x3 = xs(2).asInstanceOf[X3]
        f(x1,x2,x3)
      }
    }
  implicit def t4[X1,X2,X3,X4,R]: ParList.Aux[(X1, X2, X3, X4), R, (ValuedNode[X1], ValuedNode[X2], ValuedNode[X3], ValuedNode[X4]), (X1, X2, X3, X4) => R]  =
    new ParList[(X1,X2,X3,X4),R]{

      override type NA = (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4])
      override type F = (X1, X2, X3, X4) => R

      override def listDeps(deps: (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4])): Seq[Node] = Seq(deps._1,deps._2,deps._3, deps._4)

      override def listFunction(f: (X1,X2,X3,X4) => R): (Seq[Any]) => R = { xs =>
        val x1 = xs(0).asInstanceOf[X1]
        val x2 = xs(1).asInstanceOf[X2]
        val x3 = xs(2).asInstanceOf[X3]
        val x4 = xs(3).asInstanceOf[X4]
        f(x1,x2,x3,x4)
      }
    }
  implicit def t5[X1,X2,X3,X4,X5,R]: ParList.Aux[(X1, X2, X3,X4,X5), R, (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5]), (X1, X2, X3,X4,X5) => R]  =
    new ParList[(X1, X2, X3, X4, X5), R]{
      override type NA = (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5])
      override type F = (X1, X2, X3,X4,X5) => R

      override def listDeps(deps: (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5])): Seq[Node] = Seq(deps._1,deps._2,deps._3, deps._4, deps._5)

      override def listFunction(f: (X1, X2, X3,X4,X5) => R): (Seq[Any]) => R = { xs =>
        val x1 = xs(0).asInstanceOf[X1]
        val x2 = xs(1).asInstanceOf[X2]
        val x3 = xs(2).asInstanceOf[X3]
        val x4 = xs(3).asInstanceOf[X4]
        val x5 = xs(4).asInstanceOf[X5]

        f(x1,x2,x3,x4,x5)
      }
    }
  implicit def t6[X1,X2,X3,X4,X5,X6,R]: ParList.Aux[(X1, X2, X3,X4,X5,X6), R, (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5],ValuedNode[X6]), (X1,X2,X3,X4,X5,X6) => R]  =
    new ParList[(X1, X2, X3, X4, X5, X6), R]{
      override type NA = (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5], ValuedNode[X6])
      override type F = (X1, X2, X3,X4,X5, X6) => R

      override def listDeps(deps: (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5], ValuedNode[X6])): Seq[Node] = Seq(deps._1,deps._2,deps._3, deps._4, deps._5, deps._6)

      override def listFunction(f: (X1, X2, X3,X4,X5,X6) => R): (Seq[Any]) => R = { xs =>
        val x1 = xs(0).asInstanceOf[X1]
        val x2 = xs(1).asInstanceOf[X2]
        val x3 = xs(2).asInstanceOf[X3]
        val x4 = xs(3).asInstanceOf[X4]
        val x5 = xs(4).asInstanceOf[X5]
        val x6 = xs(5).asInstanceOf[X6]

        f(x1,x2,x3,x4,x5,x6)
      }
    }
  implicit def t7[X1,X2,X3,X4,X5,X6,X7,R]: ParList.Aux[(X1, X2, X3,X4,X5,X6,X7), R, (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5],ValuedNode[X6],ValuedNode[X7]), (X1,X2,X3,X4,X5,X6,X7) => R]  =
    new ParList[(X1, X2, X3, X4, X5, X6,X7), R]{
      override type NA = (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5], ValuedNode[X6], ValuedNode[X7])
      override type F = (X1, X2, X3,X4,X5, X6, X7) => R

      override def listDeps(deps: (ValuedNode[X1], ValuedNode[X2],ValuedNode[X3],ValuedNode[X4],ValuedNode[X5], ValuedNode[X6],ValuedNode[X7])): Seq[Node] = Seq(deps._1,deps._2,deps._3, deps._4, deps._5, deps._6, deps._7)

      override def listFunction(f: (X1, X2, X3,X4,X5,X6,X7) => R): (Seq[Any]) => R = { xs =>
        val x1 = xs(0).asInstanceOf[X1]
        val x2 = xs(1).asInstanceOf[X2]
        val x3 = xs(2).asInstanceOf[X3]
        val x4 = xs(3).asInstanceOf[X4]
        val x5 = xs(4).asInstanceOf[X5]
        val x6 = xs(5).asInstanceOf[X6]
        val x7 = xs(6).asInstanceOf[X7]

        f(x1,x2,x3,x4,x5,x6,x7)
      }
    }
}

/** @tparam R result type of function type F
  * @tparam A argument product type (not wrapped as Node)
  */
trait ParList[A,+R] extends Serializable {
  /** A wrapped in node product type. */
  type NA
  type F
  def listFunction(f: F): Seq[Any] => R
  def listDeps(deps: NA): Seq[Node]
}