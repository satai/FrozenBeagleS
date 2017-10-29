package cz.nekola.frozenbeagle

import org.scalatest._
import org.scalatest.Matchers._

class DnaSpec extends FunSpec {
  describe("Arithmetic") {
    it("add two numbers") {
      (1 + 1) should equal (2)
    }
    it("add three numbers"){
      (1 + 1 + 1) should equal (3)
    }
  }
}
