package cz.nekola.frozenbeagle

import cz.nekola.frozenbeagle.SimulationConstants.probabilityAlleleMutation

import scala.util.Random

case class Allelle ( effect: PhenotypeChange
                   , dominantEffect: PhenotypeChange
                   ) {
  def mutate(newAllelleFactory: => Allelle): Allelle =
    if (Random.nextDouble() < probabilityAlleleMutation) {
        newAllelleFactory
    } else {
        this
    }


  override def toString = "{" ++ effect.toString ++ "|" ++ dominantEffect.toString ++ "}"
}

object Allelle {

   val paretoDistribution = new org.apache.commons.math3.distribution.ParetoDistribution(1.5, 1.5)

   def newAllelle(pleiProbability: Double
                  , negDominanceProbability: Double
                 ): Allelle = {
     val pc1 = if (Random.nextDouble() < pleiProbability) {
       PhenotypeChange.randomPhenotypeChange
     } else {
       PhenotypeChange.randomPhenotypeChangeWithOneNonZero
     }
     val pc2 = if (Random.nextDouble() < negDominanceProbability) {
       val coefficient = paretoDistribution.sample()
       PhenotypeChange(pc1.components.map(_ * (-coefficient)))
     } else {
       pc1
     }

     Allelle(pc1, pc2)
   }


}

case class DnaString (genes: Array[Allelle]) {
  def mutate(newAllelleFactory: => Allelle) = DnaString (this.genes.map{_.mutate(newAllelleFactory)})

  def randomPoint: Int = Random.nextInt(this.genes.length - 2) + 1

  override def toString = this.genes.mkString("[", ", ", "]")

  override def equals(that: Any): Boolean = that.isInstanceOf[DnaString] && that.asInstanceOf[DnaString].genes.sameElements(this.genes)

  override def hashCode(): Int = genes.deep.hashCode()
}

object DnaString {
    def crossover(dna1: DnaString, dna2: DnaString): DnaString = crossoverWithPoint(dna1.randomPoint, dna1, dna2)

    def crossoverWithPoint(crossoverPoint: Int, dna1: DnaString, dna2: DnaString): DnaString =
        DnaString(dna1.genes.take(crossoverPoint) ++ dna2.genes.drop(crossoverPoint))

    def randomDnaString(randomAllelle: => Allelle) = DnaString ((1 to 32).map(_ => randomAllelle).toArray)
}
