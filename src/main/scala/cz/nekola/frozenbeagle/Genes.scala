package cz.nekola.frozenbeagle

import cz.nekola.frozenbeagle.PhenotypeChange.randomPhenotypeChange
import cz.nekola.frozenbeagle.SimulationConstants.probabilityAlleleMutation

import scala.util.Random

case class Allelle ( effect: PhenotypeChange
                    , dominantEffect: PhenotypeChange
                    ) {
  def mutate: Allelle =
    if (Random.nextDouble() < probabilityAlleleMutation) {
        randomAllelle
    } else {
        this
    }

    def randomAllelle = {
        Allelle(randomPhenotypeChange, randomPhenotypeChange) //FIXME
    }

    override def toString = "{" ++ effect.toString ++ "|" ++ dominantEffect.toString ++ "}"
}

case class DnaString (genes: List[Allelle]) {
  def mutate = DnaString (this.genes.map{_.mutate})

  def randomPoint: Int = Random.nextInt(this.genes.length - 2) + 1

  override def toString = this.genes.mkString("[", ", ", "]")
}

object DnaString {
    def crossover(dna1: DnaString, dna2: DnaString): DnaString = crossoverWithPoint(dna1.randomPoint, dna1, dna2)

    def crossoverWithPoint(crossoverPoint: Int, dna1: DnaString, dna2: DnaString): DnaString =
        DnaString(dna1.genes.take(crossoverPoint) ++ dna2.genes.drop(crossoverPoint))
}

object Allelle {
}