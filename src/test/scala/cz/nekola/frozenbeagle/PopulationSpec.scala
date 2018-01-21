package cz.nekola.frozenbeagle

import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, _}
import Generators._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop}

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

    describe("chosen pairs") {
      it("there are no chosen pairs when choosing from empty population") {
        Population(0, Set()).chosenPairs(1).isEmpty
      }

      it("there are some chosen pairs when choosing 1 from non empty population") {
        check { (dna: DnaString, p: Phenotype) =>
          Population(0, Set( Individual(M, 1, (dna, dna), p)
                           , Individual(F, 0, (dna, dna), p)
                           )
          ).chosenPairs(1).nonEmpty
        }
      }

      it("there are thee chosen pairs when choosing 3 from population of 4 males and 5 females") {
        check { (dna: DnaString, p: Phenotype) =>
          Population(0, Set( Individual(M, 0, (dna, dna), p)
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
            val smaller = Math.min(f1(), f2())
            val bigger = Math.max(f1(), f2())
            p.chosenPairs(smaller).size <= p.chosenPairs(bigger).size
        }
      }

      it("choice of n pairs is not bigger than n") {
        check {
          (c: Choice, p: Population) =>
            p.chosenPairs(c()).size <= c()
        }

      }
    }

    /*
    describe "chosenPairs" $ do
        it "there are no chosen pairs when chosing from empty population" $
            property $ forAll (choose (1, 33)) (\fraction i ->
                let chosen = fst $ sampleState (chosenPairs fraction []) $ mkStdGen i
                in
                    [] == chosen
                )

        it "there is less chosen pairs when choosing a smaller fraction from a population" $
            property $ forAll (choose (3, 33)) (\fraction i p ->
            let
                chosen1 = fst $ sampleState (chosenPairs fraction p) $ mkStdGen i
                chosen2 = fst $ sampleState (chosenPairs 2        p) $ mkStdGen i
            in
                length chosen1 <= length chosen2
)
    * */
  }
}
