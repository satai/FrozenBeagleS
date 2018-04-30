package cz.nekola.frozenbeagle.cli

import java.lang.System.currentTimeMillis
import java.text.SimpleDateFormat

import cz.nekola.frozenbeagle.DnaString.randomDnaString
import cz.nekola.frozenbeagle.Naturalists.notes
import cz.nekola.frozenbeagle.SimulationConstants.{epochCount, epochLength}
import cz.nekola.frozenbeagle._

import scala.collection.parallel.mutable.ParArray
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
    val ts = currentTimeMillis
    val params = Params(args)
    val results = runSimulations(params)
    val duration = (currentTimeMillis - ts) / 1000
    printResult(params, ts, results, duration)
  }

  private def runSimulations(params: Params) = {
    (1 to params.simulationCount()).toParArray.map {
      _ => runSimulation(params)
    }
  }

  private def runSimulation(params: Params) = {
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
      .scanLeft(initialPopulation) { (pop, _) => tng(pop) }
      .filter(pop => pop.generation % 100 == 0)
      .map(notes(naturalists)(_))
  }

  private def printResult(params: Params, ts: Long, results: ParArray[Stream[Map[String, Double]]], duration: Long) = {
    import play.api.libs.json._
    implicit val paramsWrites: OWrites[Params] = Json.writes[Params]
    implicit val resultWrites: OWrites[Result] = Json.writes[Result]

    val output = Result(
      results.toArray.toSeq
      , params
      , duration.asInstanceOf[Double]
      , new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").format(ts)
    )

    println(Json.prettyPrint(Json.toJson(output)))
  }
}
