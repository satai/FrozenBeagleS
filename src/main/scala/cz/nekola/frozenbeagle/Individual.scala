package cz.nekola.frozenbeagle

import scala.util.Random

case class Individual ( sex: Sex
                      , birthGeneration: Int
                      , chromosomes: (DnaString, DnaString)
                      , phenotype: Phenotype
                      )

object Individual {

  import scala.util.Random._

  def randomOffspring(simulationRound: Int, father: Individual, mother: Individual) : Individual = {
    val sex = if (nextBoolean()) F else M

    val (i1, i2) = if (Random.nextBoolean) (mother, father) else (father, mother)

    val c11 = i1.chromosomes._1
    val c12 = i1.chromosomes._2
    val c21 = i2.chromosomes._1
    val c22 = i2.chromosomes._2

    val c1 = DnaString.crossover(c11, c21)
    val c2 = DnaString.crossover(c12, c22)

    Individual(sex, simulationRound, (c1, c2), Expression.expression(sex)(c1, c2))
  }
}
