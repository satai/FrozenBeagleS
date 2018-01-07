package cz.nekola.frozenbeagle

import scala.util.Random.shuffle

case class Population( generation: Int
                     , individuals: Set[Individual]
                     ) {
  def males: Set[Individual] = individuals.filter(_.sex == M)
  def females: Set[Individual] = individuals.filter(_.sex == F)

  def chosenPairs(toChoose: Int): Set[(Individual, Individual)] = {
    shuffle(
      males.zip(females)
    ).take(toChoose)
  }
}

trait PopulationChange  {
  def apply(individuals: Set[Individual]): Set[Individual]
}

trait Selection extends PopulationChange
trait Mutation extends PopulationChange
trait Breeding extends PopulationChange

object AllSurvive extends Selection {
  override def apply(individuals: Set[Individual]): Set[Individual] = individuals
}
