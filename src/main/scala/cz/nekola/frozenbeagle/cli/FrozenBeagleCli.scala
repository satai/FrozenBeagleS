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


    val ts = currentTimeMillis

    val results = (1 to params.simulationCount()).toParArray.map {
      _ =>

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

        val initialPopulation = Population(
              0
           , (1 to params.populationSize()).map(
               _ => randomIndividual(params.ratioOfPleiotropicRules(), params.ratioOfNegativeDominantRules())
             )
           )

        (1 to epochCount * epochLength).toStream
          .scanLeft(initialPopulation){(pop, _) => tng(pop)}
          .filter(pop => pop.generation % 100 == 0)
          .map(notes(naturalists)(_))
          .toSeq
    }


    results.map(
        r => println(s"${r.head} ${r(246)} ${r.last}")
    )

    println(currentTimeMillis - ts)

    import play.api.libs.json._

  }

}
