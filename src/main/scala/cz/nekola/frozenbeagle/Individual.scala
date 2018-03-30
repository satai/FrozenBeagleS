package cz.nekola.frozenbeagle

import cz.nekola.frozenbeagle.Phenotype.fitness

import scala.util.Random

case class Individual ( sex: Sex
                      , birthGeneration: Int
                      , chromosomes: (DnaString, DnaString)
                      , phenotype: Phenotype
                      )

object Individual {

  import scala.util.Random._

  def apply(sex: Sex
            , birthGeneration: Int
            , chromosomes: (DnaString, DnaString)
           ): Individual = {
    new Individual(sex, birthGeneration, chromosomes, Expression.expression(sex)(chromosomes))
  }

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

  //FIXME missing tests
  def mate(simulationRound: Int)(optimum: Phenotype) (father: Individual, mother: Individual) : Set[Individual] = {
    val p1 = fitness(optimum)(father.phenotype)
    val p2 = fitness(optimum)(father.phenotype)

    val avgPairFitness = (p1 + p2) / 2.0

    val probabilityToHaveChild = avgPairFitness

    if (nextDouble() <= probabilityToHaveChild) {
      val child = randomOffspring(simulationRound, father, mother)
      Set(child)
    } else {
      Set()
    }
  }
}
