package cz.nekola.frozenbeagle

case class Allelle ( effect: PhenotypeChange
                    , dominantEffect: PhenotypeChange
                    ) {

  override def toString = "{" ++ effect.toString ++ "|" ++ dominantEffect.toString ++ "}"
}

case class DnaString (genes: List[Allelle]) {
  override def toString = this.genes.mkString("[", ", ", "]")
}

object Allelle {
}