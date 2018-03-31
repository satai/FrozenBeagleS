package cz.nekola.frozenbeagle

import cz.nekola.frozenbeagle.PhenotypeChange.randomPhenotypeChange
import cz.nekola.frozenbeagle.SimulationConstants.probabilityAlleleMutation

import scala.util.Random

case class Allelle ( effect: PhenotypeChange
                   , dominantEffect: PhenotypeChange
                   ) {
  def mutate: Allelle =
    if (Random.nextDouble() < probabilityAlleleMutation) {
        Allelle.randomAllelle
    } else {
        this
    }


  override def toString = "{" ++ effect.toString ++ "|" ++ dominantEffect.toString ++ "}"
}

object Allelle {

 def randomAllelle = {
    Allelle(randomPhenotypeChange, randomPhenotypeChange) //FIXME
  }
}

case class DnaString (genes: Array[Allelle]) {
  def mutate = DnaString (this.genes.map{_.mutate})

  def randomPoint: Int = Random.nextInt(this.genes.length - 2) + 1

  override def toString = this.genes.mkString("[", ", ", "]")

  override def equals(that: Any): Boolean = that.isInstanceOf[DnaString] && that.asInstanceOf[DnaString].genes.sameElements(this.genes)

  override def hashCode(): Int = genes.deep.hashCode()
}

object DnaString {
    def crossover(dna1: DnaString, dna2: DnaString): DnaString = crossoverWithPoint(dna1.randomPoint, dna1, dna2)

    def crossoverWithPoint(crossoverPoint: Int, dna1: DnaString, dna2: DnaString): DnaString =
        DnaString(dna1.genes.take(crossoverPoint) ++ dna2.genes.drop(crossoverPoint))

    def randomDnaString = DnaString ((1 to 32).map(_ => Allelle.randomAllelle).toArray)
}
