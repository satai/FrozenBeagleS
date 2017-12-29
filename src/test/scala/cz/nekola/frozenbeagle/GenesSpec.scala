package cz.nekola.frozenbeagle

import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, _}

import scala.collection.immutable.List

class GenesSpec extends FunSpec with Matchers with Checkers {
  describe("Alleles") {
    it("Allele has sane text representation like '{Δ(0.0, 1.0, 2.0, -3.4)|Δ(0.0, 0.0, 0.0, 11.1)}'") {
      Allelle(
        PhenotypeChange (List(0, 1, 2, -3.4)),
        PhenotypeChange (List(0, 0, 0, 11.1))
      ).toString should equal("{Δ(0.0, 1.0, 2.0, -3.4)|Δ(0.0, 0.0, 0.0, 11.1)}")
    }
  }

  describe("DNA") {
    it("has sane text representation like '[{Δ(1.1, 1.2)|Δ(-1.2, 0.3)}, {Δ(-1.2, 0.3)|Δ(-0.2, 0.3)}]'") {
        DnaString (List ( Allelle(PhenotypeChange(List(1.1, 1.2)), PhenotypeChange(List(-1.2, 0.3)))
                        , Allelle(PhenotypeChange(List(-1.2, 0.3)), PhenotypeChange(List(-0.2, 0.3)))
                        )).toString should be("[{Δ(1.1, 1.2)|Δ(-1.2, 0.3)}, {Δ(-1.2, 0.3)|Δ(-0.2, 0.3)}]")

    }
  }
}
