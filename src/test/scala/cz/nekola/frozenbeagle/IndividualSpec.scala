package cz.nekola.frozenbeagle

import cz.nekola.frozenbeagle.Generators._
import cz.nekola.frozenbeagle.Individual.randomOffspring
import org.scalatest.prop.Checkers
import org.scalatest.{FunSpec, Matchers}

class IndividualSpec extends FunSpec with Matchers with Checkers  {

  describe("random offspring") {
    it ("birth generation is current simulation round") {
      check {
        (p1: Individual, p2: Individual, gen: Int) => randomOffspring(gen, p1, p2).birthGeneration == gen
      }
    }

    it ("has random sex") {
      val parent1 = genIndividual.sample.get
      val parent2 = genIndividual.sample.get

      val daughters= (1 to 100).map{_ => randomOffspring(1, parent1, parent2)}.filter{ i => i.sex == F}

      (daughters.size <= 70) shouldBe true //TODO better constants
      (daughters.size >= 30) shouldBe true
    }
  }

}
