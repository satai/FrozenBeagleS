package cz.nekola.frozenbeagle

import scala.util.Random

case class EvolutionRules(populationChanges: Array[Int => PopulationChange]) extends AnyVal


//FIXME test it
object EvolutionRules {

  val firstOptimum = Phenotype ((1 to SimulationConstants.dimensionCount).map(_ => 12 * Random.nextGaussian()).toArray)
  val optimumChange = PhenotypeChange ((1 to SimulationConstants.dimensionCount).map(_ => 12 * Random.nextGaussian()).toArray)

  def optimumForGen(gen: Int): Phenotype =
    if (gen < 512) firstOptimum
    else firstOptimum + optimumChange

  def apply( maximumAge: Int
           , populationSize: Int
           ): EvolutionRules = {
    new EvolutionRules(Array(
      gen => PanmicticOverlap(optimumForGen(gen))(gen)
    , _   => Turbidostat(populationSize, 0.0, maximumAge)
    , gen => DeathByAge(maximumAge, gen)
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
