package cz.nekola.frozenbeagle

object SimulationConstants {
  val dimensionCount: Int = 4

  val zeroPhenotypeVec: List[Double] = List.fill(dimensionCount) (0.0)

  val fitnessDecreaseCoefficient: Double = -0.005
}
