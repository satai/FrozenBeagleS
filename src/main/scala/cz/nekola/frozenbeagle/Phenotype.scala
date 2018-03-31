package cz.nekola.frozenbeagle

import java.lang.Math.{exp, sqrt}

import cz.nekola.frozenbeagle.SimulationConstants.{dimensionCount, fitnessDecreaseCoefficient, zeroPhenotypeVec}

import scala.util.Random._

case class Phenotype(components: Array[Double]) {
  def distance(that: Phenotype): Double =
      sqrt( (this.components, that.components)
             .zipped
             .map((x, y) => x - y)
             .map((x) => x * x)
             .sum)

  override def toString = this.components.mkString("(", ", ", ")")

  def +(phenotypeChange: PhenotypeChange): Phenotype = {
    Phenotype (this.components.zip(phenotypeChange.components).map(cs => cs._1 + cs._2))
  }

  override def equals(that: Any): Boolean = that.isInstanceOf[Phenotype] && that.asInstanceOf[Phenotype].components.sameElements(this.components)
}

case class PhenotypeChange(components: Array[Double]) {
  override def toString = this.components.mkString("Δ(", ", ", ")")

  def +(phenotypeChange: PhenotypeChange): PhenotypeChange = {
    PhenotypeChange (this.components.zip(phenotypeChange.components).map(cs => cs._1 + cs._2))
  }

  override def equals(that: Any): Boolean = that.isInstanceOf[PhenotypeChange] && that.asInstanceOf[PhenotypeChange].components.sameElements(this.components)
}

object Phenotype {
  val zeroPhenotype = Phenotype (zeroPhenotypeVec)

  def fitness(optimum: Phenotype)(phenotype: Phenotype): Double = {
    val d = phenotype distance optimum
    exp(fitnessDecreaseCoefficient * d * d)
  }
}

object PhenotypeChange {
  def randomPhenotypeChange = PhenotypeChange ((1 to dimensionCount).map(_ => nextGaussian()).toArray)

  val zeroPhenotypeChange = PhenotypeChange(zeroPhenotypeVec)

  def randomPhenotypeChangeWithOneNonZero = {
    val cs : List[Double] = nextGaussian() :: (for {_ <- 1 until dimensionCount } yield 0.0).toList
    PhenotypeChange (shuffle(cs).toArray)
  }
}