package cz.nekola.frozenbeagle

import java.lang.Math.sqrt

import cz.nekola.frozenbeagle.Phenotype.fitness
import cz.nekola.frozenbeagle.SimulationConstants.dimensionCount
import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}

class PhenotypeSpec extends FunSpec with Matchers with Checkers {

  lazy val genPhenotype: Gen[Phenotype] = for {
    x1 <- arbitrary[Double]
    x2 <- arbitrary[Double]
    x3 <- arbitrary[Double]
    x4 <- arbitrary[Double]
  } yield Phenotype(List(x1, x2, x3, x4))

  implicit lazy val arbConsumer: Arbitrary[Phenotype] = Arbitrary(genPhenotype)

  describe("Phenotype") {

    describe("Distance") {
      it("is euclidean one by example") {
        Phenotype(List(-1.0, 0.0, 0.0, 0.0)) distance Phenotype(List(0.0, 1.0, 0.0, 0.0)) should
          equal(sqrt(2))
      }

      it("distance of a phenotype to itself is zero") {
        check {
          (p: Phenotype) =>
            0.0 == (p distance p)
        }
      }

      it("distance of a phenotypes is greater or equals than zero") {
        check {
          (p1: Phenotype, p2: Phenotype) =>
            (p1 distance p2) == (p2 distance p1)
        }
      }

      it("distance of a phenotypes is symetric") {
        check {
          (p1: Phenotype, p2: Phenotype) =>
            0.0 <= (p1 distance p2)
        }
      }
    }

    describe("string representation") {
      it("looks like (1.0, 2.0, -0.3)") {
        "(1.0, 2.0, -0.3)" should equal(
          Phenotype(List(1.0, 2.0, -0.3)).toString
        )
      }
    }

    describe("zero phenotype") {
      it("contains zeroes only") {
        Phenotype.zeroPhenotype.components.forall(0.0 ==) should be(true)
      }

      it("has the dimension equal dimensionCount") {
        Phenotype.zeroPhenotype.components.length should be(dimensionCount)
      }
    }

    describe("fitness") {
      it("increasing distance to the optimum decreases fitness and via versa") {
        check {
          (optimum: Phenotype, p1: Phenotype, p2: Phenotype) =>
            (((fitness(optimum)(p1) ==  0.0) && (fitness(optimum)(p2) == 0.0) &&
             ((optimum distance p1) > 100.0) && ((optimum distance p2) > 100.0)) //FIXME
            ) || (
              ((optimum distance p1) compare fitness(optimum)(p2)) ==
              ((optimum distance p2) compare fitness(optimum)(p1))
            )
        }
      }
    }
  }
}
