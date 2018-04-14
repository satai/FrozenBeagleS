package cz.nekola.frozenbeagle.cli

import java.lang.System.currentTimeMillis

import cz.nekola.frozenbeagle.DnaString.randomDnaString
import cz.nekola.frozenbeagle._

import scala.util.Random


object FrozenBeagleCli {

  def newAllelle( pleiProbability: Double
                , negDominanceProbability: Double
                ): Allelle = {
    val pc1 = if (Random.nextDouble() < pleiProbability) {
      PhenotypeChange.randomPhenotypeChange
    } else {
      PhenotypeChange.randomPhenotypeChangeWithOneNonZero
    }

    val pc2 = if (Random.nextDouble() < negDominanceProbability) {
      PhenotypeChange (pc1.components.map(_ * (-3)))  //FIXME coeficient
    } else {
      pc1
    }

    Allelle (pc1, pc2)
  }

  val naturalists: Seq[Naturalist]= Seq(Demograph)

  def randomIndividual( pleiProbability: Double
                      , negDominanceProbability: Double
                      ): Individual = Individual(
      sex = if (Random.nextBoolean()) F else M
    , birthGeneration = 0
    , chromosomes = ( randomDnaString( newAllelle(pleiProbability, negDominanceProbability))
                    , randomDnaString( newAllelle(pleiProbability, negDominanceProbability))
                    )
    )

  def notes(population: Population): Map[String, Double] =
    naturalists.map(naturalist => naturalist.observe(population))
               .foldLeft(Map[String, Double]())(
        (a: Map[String, Double], b: Map[String, Double]) => {
          assert(a.keySet.intersect(b.keySet).isEmpty, "Two naturalists should not share a key " + a.keySet.intersect(b.keySet))
          a ++ b
        }
  )

  def main(args: Array[String]): Unit = {
    val pleiProbability = 0.1
    val negDominanceProbability = 0.45

    val evolutionRules = EvolutionRules(
         maximumAge = 64
       , populationSize = 1024
       , mutationProbability = 0.0001
       , newAllelleFactory = newAllelle(pleiProbability, negDominanceProbability )
       )
    val tng = Evolution.step(evolutionRules) _

    val initialPopulation = Population( 0
                                      , (1 to 1024).map(_ => randomIndividual(pleiProbability, negDominanceProbability))
                                      )

    val ts = currentTimeMillis

    val result = (1 to 1024 * 64).toStream.scanLeft(initialPopulation)((pop, _) => tng(pop)).map(notes)

    import play.api.libs.json._
    println(Json.prettyPrint(Json.toJson(result)))

    println(result.size)
    println(result.head)
    println(result.last)
    println(result.getClass)
    println("Time: " + ((currentTimeMillis - ts) / 1000))
  }

}
