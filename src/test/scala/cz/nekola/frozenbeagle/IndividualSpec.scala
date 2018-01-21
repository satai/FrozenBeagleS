package cz.nekola.frozenbeagle

import cz.nekola.frozenbeagle.Generators._
import cz.nekola.frozenbeagle.Individual.randomOffspring
import org.scalacheck.Prop.forAllNoShrink
import org.scalatest.prop.Checkers
import org.scalatest.{FunSpec, Matchers}

class IndividualSpec extends FunSpec with Matchers with Checkers  {

  describe("random offspring") {
    it ("birth generation is current simulation round") {
      check {
        forAllNoShrink {
          (i1:  Individual, i2: Individual, gen: Int) => randomOffspring(gen, i1, i2).birthGeneration == gen
        }
      }
    }

    it ("has random sex") {
      val parent1 = genIndividual.sample.get
      val parent2 = genIndividual.sample.get

      val daughters= (1 to 1600).map{_ => randomOffspring(1, parent1, parent2)}.filter{ i => i.sex == F}

      (daughters.size <= 1600 / 2 * 1.05) shouldBe true
      (daughters.size >= 1600 / 2 / 1.05) shouldBe true
    }
  }

}
