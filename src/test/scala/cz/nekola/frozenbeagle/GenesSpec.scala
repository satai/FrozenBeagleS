package cz.nekola.frozenbeagle

import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, _}

import Generators._

class GenesSpec extends FunSpec with Matchers with Checkers {
  describe("Alleles") {
    it("Allele has sane text representation like '{Δ(0.0, 1.0, 2.0, -3.4)|Δ(0.0, 0.0, 0.0, 11.1)}'") {
      Allelle(
        PhenotypeChange (Array(0, 1, 2, -3.4)),
        PhenotypeChange (Array(0, 0, 0, 11.1))
      ).toString should equal("{Δ(0.0, 1.0, 2.0, -3.4)|Δ(0.0, 0.0, 0.0, 11.1)}")
    }
  }

  describe("DNA") {
    it("has sane text representation like '[{Δ(1.1, 1.2)|Δ(-1.2, 0.3)}, {Δ(-1.2, 0.3)|Δ(-0.2, 0.3)}]'") {
        DnaString (Array ( Allelle(PhenotypeChange(Array(1.1, 1.2)), PhenotypeChange(Array(-1.2, 0.3)))
                         , Allelle(PhenotypeChange(Array(-1.2, 0.3)), PhenotypeChange(Array(-0.2, 0.3)))
                         )).toString should be("[{Δ(1.1, 1.2)|Δ(-1.2, 0.3)}, {Δ(-1.2, 0.3)|Δ(-0.2, 0.3)}]")

    }

    it ("mutated DNA has same length as dna before mutation") {
      check {
        (dna: DnaString) =>
          dna.genes.length == dna.mutate(Allelle.newAllelle(0.1, 0.2)).genes.length
      }
    }

    it ("mutated DNAs sometimes differ from original DNAs") {
      val dnas = (1 to 40000).map{_ => genDnaString}.map{it => it.sample.get}
      val mutated = dnas.map(it => it.mutate(Allelle.newAllelle(0.1, 0.2)))

      (dnas.toSet.diff(mutated.toSet).size > 3) should be(true)
    }

    it ("mutated DNAs usually doesn't differ from original DNAs") {
      val dnas = (1 to 10000).map{_ => genDnaString}.map{it => it.sample.get}
      val mutated = dnas.map(it => it.mutate(Allelle.newAllelle(0.1, 0.3)))

      (dnas.toSet.diff(mutated.toSet).size < 3000) should be(true)
    }

    it ("DNA after crossover has the same length as original DNAs") {
      check {
        (dna1: DnaString, dna2: DnaString) =>
          DnaString.crossover(dna1, dna2).genes.length == dna1.genes.length
      }
    }

    it ("DNA after crossover has the same beginning as the first DNA") {
      check {
        (dna1: DnaString, dna2: DnaString) =>
          DnaString.crossoverWithPoint(2, dna1, dna2).genes.take(2) sameElements dna1.genes.take(2)
      }
    }

    it ("DNA after crossover has the same ending as the second DNA") {
      check {
        (dna1: DnaString, dna2: DnaString) =>
          DnaString.crossoverWithPoint(2, dna1, dna2).genes.drop(2) sameElements dna2.genes.drop(2)
      }
    }

    it ("randomPoint can be in any point in between ") {
      (1 to 10000).map{_ => genDnaString.sample.get.randomPoint}.toSet should equal ((1 to 3).toSet)  //FIXME
    }

  }
}
