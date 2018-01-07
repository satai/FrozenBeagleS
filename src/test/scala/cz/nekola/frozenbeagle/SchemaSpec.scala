package cz.nekola.frozenbeagle

import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, _}
import Generators._

class SchemaSpec extends FunSpec with Matchers with Checkers {
  describe("Schema") {

    it("has sane text representation like <{Δ(1.0)|Δ(2.0)}{Δ(1.0)|Δ(2.0)}*{Δ(3.0)|Δ(1.0)}*>") {
      Schema (List ( Some (Allelle (PhenotypeChange(List(1.0)), PhenotypeChange(List (2.0))))
                   , Some (Allelle (PhenotypeChange(List(-4.0)), PhenotypeChange(List (2.0))))
                   , None
                   , Some (Allelle (PhenotypeChange(List(3.0)), PhenotypeChange(List (1.0))))
                   , None
                   )).toString should be("<{Δ(1.0)|Δ(2.0)}{Δ(-4.0)|Δ(2.0)}*{Δ(3.0)|Δ(1.0)}*>")
    }

    it("order is number of specified positions by example") {
      Schema (List ( Some (Allelle (PhenotypeChange(List(1.0)), PhenotypeChange(List (2.0))))
                   , Some (Allelle (PhenotypeChange(List(-4.0)), PhenotypeChange(List (2.0))))
                   , None
                   , Some (Allelle (PhenotypeChange(List(3.0)), PhenotypeChange(List (1.0))))
                   , None
                   )).order should be(3)
    }

    it("order is schemas length minus count of *") {
      check { (s: Schema) =>
        s.length == s.order + s.elements.count(_.isEmpty)
      }
    }
  }
}
