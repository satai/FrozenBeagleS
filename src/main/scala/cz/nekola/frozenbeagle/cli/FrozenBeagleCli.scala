package cz.nekola.frozenbeagle.cli

import java.lang.System.currentTimeMillis

import cz.nekola.frozenbeagle.DnaString.randomDnaString
import cz.nekola.frozenbeagle.Naturalists.notes
import cz.nekola.frozenbeagle.SimulationConstants.{epochCount, epochLength}
import cz.nekola.frozenbeagle._

import scala.util.Random


object FrozenBeagleCli {




  val naturalists: Seq[Naturalist]= Seq(Demograph)

  def randomIndividual( pleiProbability: Double
                      , negDominanceProbability: Double
                      ): Individual = Individual(
      sex = if (Random.nextBoolean()) F else M
    , birthGeneration = 0
    , chromosomes = ( randomDnaString( Allelle.newAllelle(pleiProbability, negDominanceProbability))
                    , randomDnaString( Allelle.newAllelle(pleiProbability, negDominanceProbability))
                    )
    )

  def main(args: Array[String]): Unit = {

    val params = Params(args)

    val evolutionRules = EvolutionRules(
         maximumAge = params.maximumAge()
       , populationSize = params.populationSize()
       , mutationProbability = params.mutationProbability()
       , newAllelleFactory = Allelle.newAllelle(
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

        (1 to epochCount * epochLength).toStream
          .scanLeft(initialPopulation){(pop, _) => tng(pop)}
          .map(notes(naturalists))
    }

    import play.api.libs.json._
    println(Json.prettyPrint(Json.toJson(results)))

    println(results.size)
    println(results.head.head)
    println(results.head.last)
    println("Time: " + ((currentTimeMillis - ts) / 1000))
  }

}
