package cz.nekola.frozenbeagle

import cz.nekola.frozenbeagle.Phenotype.zeroPhenotype

object Expression{
  def expression(sex: Sex)(dnaStrings: (DnaString, DnaString)): Phenotype = {
    val dna1 = dnaStrings._1
    val dna2 = dnaStrings._2

    val changes = dna1.genes.zip(dna2.genes).map { t => if (t._1 == t._2) t._1.dominantEffect
                                                        else t._1.effect + t._2.effect
                                                 }
    changes.foldLeft(zeroPhenotype){ (p: Phenotype, pc: PhenotypeChange) => p + pc}
  }
}

