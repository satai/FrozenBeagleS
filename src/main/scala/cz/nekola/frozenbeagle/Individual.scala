package cz.nekola.frozenbeagle

case class Individual ( sex: Sex
                      , birthGeneration: Int
                      , chromosomes: (DnaString, DnaString)
                      , phenotype: Phenotype
                      )

object Individual {

  import scala.util.Random._

  def randomOffspring(simulationRound: Int, i1: Individual, i2: Individual) : Individual = {
    val sex = if (nextBoolean()) F else M

    Individual(sex, simulationRound, i1.chromosomes, i1.phenotype)
  }
}
