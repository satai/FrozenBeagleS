package cz.nekola.frozenbeagle

import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, _}

import Generators._

class PopulationSpec extends FunSpec with Matchers with Checkers {
  describe("Population") {

    it("can be divided into males and females") {
      check {
        (p: Population) =>
          p.individuals == p.males ++ p.females
      }
    }

    it("there is no individual both male and female") {
      check {
        (p: Population) =>
          p.males.intersect(p.females).isEmpty
      }
    }

    it("all males are males and all females are females") {
     check {
       (p: Population) =>
         p.females.forall{_.sex == F} && p.males.forall{_.sex == M}
     }
    }

    describe("all survive selection") {
      it ("doesn't change the population") {
        check {
          (p: Population) =>
            AllSurvive(p.individuals) == p.individuals
        }
      }
    }
  }
}
