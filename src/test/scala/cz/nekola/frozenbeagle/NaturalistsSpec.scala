package cz.nekola.frozenbeagle

import org.scalatest.prop.Checkers
import org.scalatest.{FunSpec, Matchers}

import Generators._

class NaturalistsSpec extends FunSpec with Matchers with Checkers {

  describe("Demograph") {
    it ("puts population size, counts of males and females and ") {
      check {
        p: Population =>
          Demograph.observe(p).keySet == Set("generation", "populationSize", "maleCount", "femaleCount")
      }
    }

  }

}
