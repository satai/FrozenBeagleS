package cz.nekola.frozenbeagle

case class EvolutionRules(populationChanges: Array[Int => PopulationChange]) extends AnyVal


//FIXME test it
object EvolutionRules {

  def optimumForGen(gen: Int) =
    if (gen < 512) Phenotype(Array(0.0, 4.2, -16, 0.1))
    else Phenotype(Array(-5, 12, -2, 9))

  def apply(maximumAge: Int
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
