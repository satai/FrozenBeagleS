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

    it ("writes te right generation into the result notebook") {
      check {
        p: Population =>
          val notebook = Demograph.observe(p)
          notebook("generation").toInt == p.generation
      }
    }


    it ("writes te right population size into result notebook") {
      check {
        p: Population =>
          val notebook = Demograph.observe(p)
          notebook("populationSize").toInt == p.individuals.size
      }
    }

    it ("population size is male count plus female count") {
      check {
        p: Population =>
          val notebook = Demograph.observe(p)
          notebook("populationSize").toInt == notebook("maleCount").toInt + notebook("femaleCount").toInt
      }
    }
  }


}
