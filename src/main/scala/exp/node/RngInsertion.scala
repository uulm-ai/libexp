package exp.node

/**
  * Created by thomas on 26.11.15.
  */
case object RngInsertion extends Stage {
  override type Payload[+T] = String

  def seed(name: String): Inject[RngInsertion.type, Long] = Inject[RngInsertion.type,Long](this,name)

  implicit def aboveBase: StageAfter[Base.type, RngInsertion.type] = StageAfter(this)
}
