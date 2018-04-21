package cz.nekola.frozenbeagle.cli

import java.lang.System.currentTimeMillis

import cz.nekola.frozenbeagle.DnaString.randomDnaString
import cz.nekola.frozenbeagle.SimulationConstants.{epochCount, epochLength}
import cz.nekola.frozenbeagle._
import org.rogach.scallop.ScallopConf

import scala.collection.immutable
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

  case class Params(arguments: Seq[String]) extends ScallopConf(arguments) {
    val simulationCount = opt[Int] (
        required = true
      , descr = "Number of simulations to run with given set of parameters."
      , noshort = true
      )

    val populationSize = opt[Int] (
        required = true
      , descr = "Size of initial population and the size of population, that is equlibrium for turbidostat."
      , noshort = true
      )

    val countOfBases = opt[Int](
        required = true
      , descr = "DNA string length (count of allellas)."
      , noshort = true
      )

    val ratioOfNegativeDominantRules = opt[Int](
        required = true
      , descr = "Ratio of negative dominant allelas as they appear in the initial population and as they are created by mutations "
      , noshort = true
      )

    val ratioOfPleiotropicRules = opt[Int](
        required = true
      , descr = "Ratio of allellas that affect multiple dimensions of the phenotype. This ratio is in the initial population and in this ratio new allellas are created by mutation."
      , noshort = true
      )

    val maximumAge = opt[Int](
        required = true
      , descr = "After this age organisms die by age."
      , noshort = true
    )

    val mutationProbability = opt[Int](
      required = true
      , descr = "FIXME"
      , noshort = true
    )

    verify()
  }

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
