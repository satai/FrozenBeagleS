package cz.nekola.frozenbeagle

import cz.nekola.frozenbeagle.PhenotypeChange.zeroPhenotypeChange
import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, _}
import Generators._
import cz.nekola.frozenbeagle.Expression.expression

class ExpressionSpec extends FunSpec with Matchers with Checkers {

  describe("Expression") {
    it("is sum of parts for non-dominant allellas") {
      check { (sex: Sex) =>
        val allelle1 = Allelle(PhenotypeChange(List(1.0, -1.0, 0.0, 0.0)), zeroPhenotypeChange)
        val allelle2 = Allelle(PhenotypeChange(List(0.0, -3.0, 1.0, 1.0)), zeroPhenotypeChange)
        val dnaString1 = DnaString((1 to 8).toList.map( _ => allelle1))
        val dnaString2 = DnaString((1 to 8).toList.map( _ => allelle2))

        expression(sex)((dnaString1, dnaString2)) == Phenotype( List(8.0, -32.0, 8.0, 8.0))
      }
    }

    it("is sum of dominant parts for non-dominant allellas") {
      check { (sex: Sex) =>
        val allelle1 = Allelle(zeroPhenotypeChange, PhenotypeChange(List(1.0, -2.0, 0.0, 0.0)))
        val allelle2 = Allelle(zeroPhenotypeChange, PhenotypeChange(List(1.0, -2.0, 0.0, 0.0)))
        val dnaString1 = DnaString((1 to 8).toList.map( _ => allelle1))
        val dnaString2 = DnaString((1 to 8).toList.map( _ => allelle2))

        expression(sex)((dnaString1, dnaString2)) == Phenotype( List(8.0, -16.0, 0.0, 0.0))
      }
    }
  }
}
