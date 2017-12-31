package cz.nekola.frozenbeagle

case class Individual ( sex: Sex
                      , birthGeneration: Int
                      , chromosomes: (DnaString, DnaString)
                      , phenotype: Phenotype
                      )