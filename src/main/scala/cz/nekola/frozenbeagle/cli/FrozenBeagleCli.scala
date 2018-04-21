package cz.nekola.frozenbeagle.cli

import java.lang.System.currentTimeMillis

import cz.nekola.frozenbeagle.DnaString.randomDnaString
import cz.nekola.frozenbeagle.SimulationConstants.{epochCount, epochLength}
import cz.nekola.frozenbeagle._

import scala.util.Random


object FrozenBeagleCli {

  val paretoDistribution = new org.apache.commons.math3.distribution.ParetoDistribution(1.5, 1.5)

  def newAllelle( pleiProbability: Double
                , negDominanceProbability: Double
  ): Allelle = {
    val pc1 = if (Random.nextDouble() < pleiProbability) {
      PhenotypeChange.randomPhenotypeChange
    } else {
      PhenotypeChange.randomPhenotypeChangeWithOneNonZero
    }
    val pc2 = if (Random.nextDouble() < negDominanceProbability) {
      val coefficient = paretoDistribution.sample()
      PhenotypeChange (pc1.components.map(_ * (- coefficient)))
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

    val params = Params(Array("--help"))

    val evolutionRules = EvolutionRules(
         maximumAge = params.maximumAge()
       , populationSize = params.populationSize()
       , mutationProbability = params.mutationProbability()
       , newAllelleFactory = newAllelle(
           params.ratioOfPleiotropicRules()
         , params.ratioOfNegativeDominantRules()
         )
       )
    val tng = Evolution.step(evolutionRules) _

    val ts = currentTimeMillis

    val results = (1 to params.simulationCount()).map {
      _ =>
        val initialPopulation = Population( 0
        , (1 to epochLength * epochCount).map(_ => randomIndividual(params.ratioOfPleiotropicRules(), params.ratioOfNegativeDominantRules()))
        )

        (1 to epochCount * epochLength).toStream.scanLeft(initialPopulation)((pop, _) => tng(pop)).map(notes)
    }

    import play.api.libs.json._
    println(Json.prettyPrint(Json.toJson(results)))

    println(results.size)
    println(results.head.head)
    println(results.head.last)
    println("Time: " + ((currentTimeMillis - ts) / 1000))
  }

}
