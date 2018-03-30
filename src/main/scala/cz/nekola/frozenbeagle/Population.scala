package cz.nekola.frozenbeagle

import scala.util.Random
import scala.util.Random.shuffle

case class Population( generation: Int
                     , individuals: Set[Individual]
                     ) {

  def males: Set[Individual] = individuals.filter(_.sex == M)
  def females: Set[Individual] = individuals.filter(_.sex == F)

  def size: Int = individuals.size
}

trait PopulationChange {
  def apply(individuals: Set[Individual]): Set[Individual]
}

case class Turbidostat(k4: Double
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

case class DeathByAge(gen: Int) extends PopulationChange {
  override def apply(individuals: Set[Individual]) = individuals.filter {
    individual => individual.birthGeneration >= gen
  }
}

object DeathByAge {
  def apply(maxAge: Int, actualGeneration: Int) = new DeathByAge(actualGeneration - maxAge)
}

case class PanmicticOverlap(optimum: Phenotype)(gen: Int) extends PopulationChange {
  override def apply(individuals: Set[Individual]) = {
    val mate = Individual.mate(gen)(optimum) _
    val pairs = PanmicticOverlap.chosenPairs(individuals)
    val newBorns = pairs.flatMap(p =>  mate(p._1, p._2))

    individuals ++ newBorns
  }
}

object PanmicticOverlap {
  def chosenPairs(population: Population): Set[(Individual, Individual)] = chosenPairs(population.individuals)

  def chosenPairs(individuals: Set[Individual], toChoose: Int = Int.MaxValue): Set[(Individual, Individual)] = {
    shuffle(individuals.filter(_.sex == F))
      .zip(shuffle(individuals.filter(_.sex == M)))
      .take(toChoose)
  }
}

object AllSurvive extends PopulationChange {
  override def apply(individuals: Set[Individual]): Set[Individual] = individuals
}
