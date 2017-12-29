package cz.nekola.frozenbeagle

case class Allelle ( effect: PhenotypeChange
                    , dominantEffect: PhenotypeChange
                    ) {
  def mutate: Allelle = Allelle (this.effect.mutate, this.dominantEffect.mutate)

  override def toString = "{" ++ effect.toString ++ "|" ++ dominantEffect.toString ++ "}"
}

case class DnaString (genes: List[Allelle]) {
  def mutate = DnaString (this.genes.map{_.mutate})

  override def toString = this.genes.mkString("[", ", ", "]")
}

object Allelle {
}