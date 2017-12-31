package cz.nekola.frozenbeagle

import cz.nekola.frozenbeagle.PhenotypeChange.randomPhenotypeChange

import scala.util.Random

case class Allelle ( effect: PhenotypeChange
                    , dominantEffect: PhenotypeChange
                    ) {
  def mutate: Allelle =
    if (Random.nextDouble() < 0.0002) { //FIXME
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

  override def toString = this.genes.mkString("[", ", ", "]")
}

object Allelle {
}