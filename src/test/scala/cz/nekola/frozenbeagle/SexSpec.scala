package cz.nekola.frozenbeagle

import org.scalatest.{Matchers, _}
import org.scalatest.prop.Checkers

class SexSpec extends FunSpec with Matchers with Checkers {
  describe("Males and Females") {
    it("there are two different values of sex: F and M") {
     F should not equal M
    }
  }
}
