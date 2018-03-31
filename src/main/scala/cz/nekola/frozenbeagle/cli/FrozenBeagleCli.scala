package cz.nekola.frozenbeagle.cli

import java.lang.System.currentTimeMillis

import cz.nekola.frozenbeagle.DnaString.randomDnaString
import cz.nekola.frozenbeagle._

import scala.util.Random


object FrozenBeagleCli {

  val naturalists: Seq[Naturalist]= Seq(Demograph)

  def randomIndividual: Individual = Individual(
      sex = if (Random.nextBoolean()) F else M
    , birthGeneration = 0
    , chromosomes = ( randomDnaString, randomDnaString)
  )

  def notes(population: Population): Map[String, Double] = naturalists.map(naturalist => naturalist.observe(population)).foldLeft(Map[String, Double]())(
    (a: Map[String, Double], b: Map[String, Double]) => {
      assert(a.keySet.intersect(b.keySet).isEmpty, "Two naturalists should not share a key " + a.keySet.intersect(b.keySet))
      a ++ b
    }
  )

  def main(args: Array[String]): Unit = {
    val evolutionRules = EvolutionRules(maximumAge = 64,  populationSize = 1024 * 8)
    val tng = Evolution.step(evolutionRules) _

    val initialPopulation = Population( 0
                                      , (1 to 1024).map(_ => randomIndividual)
                                      )

    val ts = currentTimeMillis

    val result = (1 to 1024 * 16).toStream.scanLeft(initialPopulation)((pop, _) => tng(pop)).map(notes)


    println(result.size)
    println(result.head)
    println(result.last)
    println(result.getClass)
    println("Time: " + ((currentTimeMillis - ts) / 1000))
  }

}
