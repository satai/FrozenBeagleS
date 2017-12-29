package cz.nekola.frozenbeagle

case class Allelle ( effect: PhenotypeChange
                    , dominantEffect: PhenotypeChange
                    ) {

  override def toString = "{" ++ effect.toString ++ "|" ++ dominantEffect.toString ++ "}"
}

object Allelle {
}