package cz.nekola.frozenbeagle

import java.lang.Math.{max, min}

import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, _}
import Generators._
import org.scalacheck.{Arbitrary, Gen}

import scala.collection.immutable.List

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
          p.females.forall {
            _.sex == F
          } && p.males.forall {
            _.sex == M
          }
      }
    }

    describe("all survive selection") {
      it("doesn't change the population") {
        check {
          (p: Population) =>
            AllSurvive(p.individuals) == p.individuals
        }
      }
    }

    describe("chosen pairs") {
      it("there are no chosen pairs when choosing from empty population") {
        Population(0, Set()).chosenPairs(1).isEmpty
      }

      it("there are some chosen pairs when choosing 1 from non empty population") {
        check { (dna: DnaString, p: Phenotype) =>
          Population(0, Set(Individual(M, 1, (dna, dna), p)
            , Individual(F, 0, (dna, dna), p)
          )
          ).chosenPairs(1).nonEmpty
        }
      }

      it("there are three chosen pairs when choosing 3 from population of 4 males and 5 females") {
        check { (dna: DnaString, p: Phenotype) =>
          Population(0, Set(Individual(M, 0, (dna, dna), p)
            , Individual(M, 1, (dna, dna), p)
            , Individual(M, 2, (dna, dna), p)
            , Individual(M, 3, (dna, dna), p)
            , Individual(F, 0, (dna, dna), p)
            , Individual(F, 1, (dna, dna), p)
            , Individual(F, 2, (dna, dna), p)
            , Individual(F, 3, (dna, dna), p)
            , Individual(F, 4, (dna, dna), p)
          )
          ).chosenPairs(3).size == 3
        }
      }

      case class Choice(v: Int) {
        def apply(): Int = this.v
      }
      implicit lazy val fraction: Arbitrary[Choice] = Arbitrary(
        for {
          c <- Gen.choose(0, 15)
        } yield Choice(c)
      )

      it("there is less or equal chosen pairs when choosing a smaller fraction from a population") {
        check {
          (f1: Choice, f2: Choice, p: Population) =>
            val smaller = min(f1(), f2())
            val bigger = max(f1(), f2())
            p.chosenPairs(smaller).size <= p.chosenPairs(bigger).size
        }
      }

      it("choice of n pairs is not bigger than n") {
        check {
          (c: Choice, p: Population) =>
            p.chosenPairs(c()).size <= c()
        }
      }

      it("choice of population size of pairs chooses number of pairs that is number of individuals of the less frequent sex") {
        check {
          (p: Population) =>
            p.chosenPairs(p.size).size == min(p.males.size, p.females.size)
        }
      }
    }

    describe("death of old") {
      it("all survivors are born after or in the specified generation") {
        check {
          (p: Population, gen: Int) =>
            p.dieBornBefore(gen).forall {
              _.birthGeneration >= gen
            }
        }
      }

      it("all survivors were members of the original population") {
        check {
          (p: Population, gen: Int) =>
            p.dieBornBefore(gen).subsetOf(p.individuals)
        }

      }
      it("keeps young enough individuals") {
        check {
          (p: Population, gen: Int) =>
            val youngling = Individual(F, gen, (DnaString(List.empty), DnaString(List.empty)), Phenotype(List.empty))
            Population(p.generation, p.individuals + youngling).dieBornBefore(gen).contains(youngling)
        }
      }
    }
  }


  describe("Turbidostat") {
      it("Computes right k4 for given population size, maximum age and accidental death probability") {
        Turbidostat(256, 0.0, 64).k4 shouldEqual 2.1545975296585646E-6
      }

      it("k5 is the same as accidental death probability") {
        Turbidostat(1256, 0.2, 11).k5 shouldEqual 0.2
      }

      it ("turbidostat constants keep population stable for pair fitness = 1.0 and population size 1024") {
        val maxAge = 64
        val turbidostat = Turbidostat(1024, 0.1, maxAge)
        val k4 = turbidostat.k4
        val k5 = turbidostat.k5
        val populationSizeAfterMating = 1024 * 1.5

        val deathProbability = k4 * populationSizeAfterMating * populationSizeAfterMating + k5

        val willDieByTurbidostat = deathProbability * populationSizeAfterMating
        val willDieOfAge = populationSizeAfterMating / maxAge

        1024 shouldEqual (populationSizeAfterMating - willDieByTurbidostat - willDieOfAge )
      }
  }
}
