package cz.nekola.frozenbeagle

import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Random
import scala.util.Random.shuffle

case class Population( generation: Int
                     , individuals: Set[Individual]
                     ) {

  def males: Set[Individual] = individuals.filter(_.sex == M)
  def females: Set[Individual] = individuals.filter(_.sex == F)

  def size: Int = individuals.size

  def chosenPairs(toChoose: Int): Set[(Individual, Individual)] = {
    shuffle(males)
      .zip(shuffle(females))
      .take(toChoose)
  }

  def dieBornBefore(gen: Int): Set[Individual] = individuals.filter{individual => individual.birthGeneration >= gen}
}

trait PopulationChange  {
  def apply(individuals: Set[Individual]): Set[Individual]
}

case class Turbidostat( k4: Double
                      , k5: Double
                      ) extends PopulationChange {

  override def apply(individuals: Set[Individual]): Set[Individual] = {
    val actualSize = individuals.size
    val survivalProbability = Math.max(0.1, 1.0 - k4 * actualSize * actualSize + k5)

    def survive(individual: Individual): Boolean = {
      Random.nextDouble() <= survivalProbability
    }

    individuals.filter(i => survive(i))
  }
}

object Turbidostat {
  def apply( expectedPopulationSize: Int
           , accidentDeathProbability: Double
           , maximumAge: Int) = {
    require(accidentDeathProbability >=0 && accidentDeathProbability <= 1.0, "randomDeathProbability should be probability")
    require(expectedPopulationSize > 0)
    require(maximumAge > 0)

    val k4 = (4 - 12 * accidentDeathProbability - (12.0 / maximumAge)) / 27.0 / expectedPopulationSize / expectedPopulationSize

    new Turbidostat(k4, accidentDeathProbability)
  }
}

object AllSurvive extends PopulationChange {
  override def apply(individuals: Set[Individual]): Set[Individual] = individuals
}
