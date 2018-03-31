package cz.nekola.frozenbeagle.cli

import java.lang.System.currentTimeMillis

import cz.nekola.frozenbeagle.DnaString.randomDnaString
import cz.nekola.frozenbeagle._

import scala.util.Random

object FrozenBeagleCli {

  def randomIndividual: Individual = Individual(
      sex = if (Random.nextBoolean()) F else M
    , birthGeneration = 0
    , chromosomes = ( randomDnaString, randomDnaString)
  )

  def main(args: Array[String]): Unit = {
    val evolutionRules = EvolutionRules(maximumAge = 64,  populationSize = 1024 * 4)
    val tng = Evolution.step(evolutionRules) _

    val initialPopulation = Population( 0
                                      , (1 to 1024).map(_ => randomIndividual)
                                      )

    val ts = currentTimeMillis

    val result = (1 to 1024 * 16).foldLeft(initialPopulation)((pop, _) => tng(pop))

    println("Time: " + ((currentTimeMillis - ts) / 1000))

    println(result.size)
    println(result.getClass)
  }

}
