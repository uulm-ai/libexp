package sampler

import scala.util.Random

case class NormalSampler(mean: Double, sd: Double, seed: Long = 0) {
  /** Estimates the mean. */
  def estimateMean: Iterator[Double] = {
    val random = new Random(seed)
    val draws = Iterator.continually(random.nextGaussian() * sd + mean)
    val accDraws = draws.scanLeft(Nil: List[Double]){case (acc,draw) => draw :: acc}
    accDraws.map(x => x.sum / x.size.toDouble).drop(1)
  }
}
