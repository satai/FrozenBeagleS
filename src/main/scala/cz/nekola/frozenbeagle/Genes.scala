package cz.nekola.frozenbeagle

case class Allelle ( effect: Phenotype
                    , dominantEffect: Phenotype
                    ) {

  override def toString = "{" ++ effect.toString ++ "|" ++ dominantEffect.toString ++ "}"
}

object Allelle {
}