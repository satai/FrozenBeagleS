package cz.nekola.frozenbeagle

import cz.nekola.frozenbeagle.SimulationConstants.{epochLength, optimumChangeSizeCoefficient, optimumSizeCoefficient}

import scala.util.Random

case class EvolutionRules(populationChanges: Array[Int => PopulationChange]) extends AnyVal


//FIXME test it
object EvolutionRules {

  val firstOptimum = Phenotype ((1 to SimulationConstants.dimensionCount).map(_ => optimumSizeCoefficient * Random.nextGaussian()).toArray)
  val optimumChange = PhenotypeChange ((1 to SimulationConstants.dimensionCount).map(_ => optimumChangeSizeCoefficient * Random.nextGaussian()).toArray)

  def optimumForGen(gen: Int): Phenotype = {
    val era: Int = gen / epochLength
    if (0 == era % 2 ) firstOptimum
    else firstOptimum + optimumChange
  }

  def apply( maximumAge: Int
           , populationSize: Int
           , mutationProbability: Double
           , newAllelleFactory: => Allelle
           ): EvolutionRules = {
    new EvolutionRules(Array(
      gen => PanmicticOverlap(optimumForGen(gen))(gen)
    , _   => Turbidostat(populationSize, 0.0, maximumAge)
    , gen => DeathByAge(maximumAge, gen)
    , _   => Mutations(mutationProbability, newAllelleFactory)
    ))
  }
}

object Evolution {

  //TODO refactor make more general
  def step(evolutionRules: EvolutionRules)(population: Population): Population = {
    val nextGen = population.generation + 1
    val changesToPerform = evolutionRules.populationChanges.map(_(nextGen))

    val tng = changesToPerform.foldLeft(population.individuals)((individuals, change) => change(individuals))

    Population(generation = nextGen, individuals = tng)
  }

}
