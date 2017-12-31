package cz.nekola.frozenbeagle

import java.lang.Math.sqrt

import cz.nekola.frozenbeagle.Phenotype.fitness
import cz.nekola.frozenbeagle.SimulationConstants.dimensionCount
import org.scalatest._
import org.scalatest.prop.Checkers
import smile.stat.distribution.{GaussianDistribution}
import smile.stat.hypothesis.KSTest

import Generators._

class PhenotypeSpec extends FunSpec with Matchers with Checkers {


  describe("Phenotypes and their changes") {

    describe("Distance") {
      it("is euclidean one by example") {
        Phenotype(List(-1.0, 0.0, 0.0, 0.0)) distance Phenotype(List(0.0, 1.0, 0.0, 0.0)) should
          equal(sqrt(2))
      }

      it("is euclidean one by an other example") {
        Phenotype(List(2.0, 0.0, 0.0, 0.0)) distance Phenotype(List(0.0, 2.0, 2.0, -2.0)) should
          equal(4)
      }

      it("distance of a phenotype to itself is zero") {
        check {
          (p: Phenotype) =>
            0.0 == (p distance p)
        }
      }

      it("distance of a phenotypes is symmetric") {
        check {
          (p1: Phenotype, p2: Phenotype) =>
            (p1 distance p2) == (p2 distance p1)
        }
      }

      it("distance of a phenotypes is greater or equals than zero") {
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

    describe("random phenotype change") {
      it("random phenotypes length is dimensionCount") {
        PhenotypeChange.randomPhenotypeChange.components.length should be(dimensionCount)
      }

      it("random phenotypes change components are in normal distribution") {
        val someValues = (1 to 10000).flatMap { _ => PhenotypeChange.randomPhenotypeChange.components }

        (KSTest.test(someValues.toArray, GaussianDistribution.getInstance()).d < 0.01) should be (true)
      }
    }

    describe("random phenotype change with one non zero") {
      it("its length is dimensionCount") {
        PhenotypeChange.randomPhenotypeChangeWithOneNonZero.components.length should be(dimensionCount)
      }

      it("its has one non-zero value") {
        PhenotypeChange.randomPhenotypeChangeWithOneNonZero.components.count(_ != 0.0) should be(1)
      }

      it("random phenotypes change components are in normal distribution") {
        val someValues = (1 to 10000).flatMap { _ => PhenotypeChange.randomPhenotypeChangeWithOneNonZero.components }
                                     .filter(_ != 0.0)

        (KSTest.test(someValues.toArray, GaussianDistribution.getInstance()).d < 0.01) should be (true)
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
