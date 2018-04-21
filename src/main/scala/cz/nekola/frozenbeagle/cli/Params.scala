package cz.nekola.frozenbeagle.cli

import org.rogach.scallop.ScallopConf

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
    , descr = "Probability, that a allelle is mutated during one round of simulation"
    , noshort = true
  )

  verify()
}
