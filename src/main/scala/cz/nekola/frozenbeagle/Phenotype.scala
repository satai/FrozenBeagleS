package cz.nekola.frozenbeagle

import java.lang.Math.{exp, sqrt}

import cz.nekola.frozenbeagle.SimulationConstants.{dimensionCount, fitnessDecreaseCoefficient, zeroPhenotypeVec}

import scala.util.Random

case class Phenotype(components: List[Double]) {
  def distance(that: Phenotype): Double =
      sqrt( (this.components, that.components)
             .zipped
             .map((x, y) => x - y)
             .map((x) => x * x)
             .sum)

  override def toString = this.components.mkString("(", ", ", ")")
}

object Phenotype {
  val zeroPhenotype = Phenotype (zeroPhenotypeVec)

  def randomPhenotype = Phenotype ((1 to dimensionCount).map(_ => Random.nextGaussian()).toList)

  def fitness(optimum: Phenotype)(phenotype: Phenotype): Double = {
    val d = phenotype distance optimum
    exp(fitnessDecreaseCoefficient * d * d)
  }
}