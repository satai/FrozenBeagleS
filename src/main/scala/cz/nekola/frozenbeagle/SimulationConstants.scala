package cz.nekola.frozenbeagle

object SimulationConstants {
  lazy val dimensionCount: Int = 4

  lazy val zeroPhenotypeVec: Array[Double] = Array.fill(dimensionCount) (0.0)

  lazy val optimumChangeGeneration: Int = 8 * 1024

  lazy val maxSteps: Int = 3 * optimumChangeGeneration

  lazy val accidentDeathProbability: Double = 0.0

  lazy val probabilityAlleleMutation: Double = 0.0002

  lazy val optimumSizeCoefficient: Double = 12.0

  lazy val optimumChangeSizeCoefficient: Double = 12.0

  lazy val fitnessDecreaseCoefficient: Double = -0.005

  lazy val negativeDominanceScale: Double = 1.5

  lazy val negativeDominanceShape: Double = 1.5
}
