package cz.nekola.frozenbeagle

case class Population( generation: Int
                     , individuals: Set[Individual]
                     ) {
  def males: Set[Individual] = individuals.filter(_.sex == M)
  def females: Set[Individual] = individuals.filter(_.sex == F)

}

trait PopulationChange  {
  def apply(individuals: Set[Individual]): Set[Individual]
}

object AllSurvive extends PopulationChange {
  override def apply(individuals: Set[Individual]): Set[Individual] = individuals
}
