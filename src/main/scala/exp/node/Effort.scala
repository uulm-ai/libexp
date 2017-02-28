package exp.node

trait Effort{
  def expectedTime: Double
}
object Effort{
  case class TimeEst(expectedTime: Double) extends Effort
  def none: TimeEst = TimeEst(0d)
  def low: TimeEst = TimeEst(0.01d)
  def moderate: TimeEst = TimeEst(1d)
  def high: TimeEst = TimeEst(10d)
}

case class Length(meanLength: Double)