package cz.nekola.frozenbeagle

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

object Generators {
  implicit lazy val arbSex: Arbitrary[Sex] = Arbitrary(Gen.oneOf(F, M))

  lazy val genPhenotype: Gen[Phenotype] = for {
    x1 <- Gen.chooseNum(-10.0, 10.0, 0.0, 0.0)
    x2 <- Gen.choose(-10.0, 10.0)
    x3 <- Gen.choose(-10.0, 10.0)
    x4 <- Gen.choose(-10.0, 10.0)
  } yield Phenotype(Array(x1, x2, x3, x4))

  implicit lazy val arbPhenotype: Arbitrary[Phenotype] = Arbitrary(genPhenotype)

  lazy val genPhenotypeChange: Gen[PhenotypeChange] = for {
    x1 <- Gen.chooseNum(-3.0, 3.0, 0.0, 0.0)
    x2 <- Gen.choose(-3.0, 3.0)
    x3 <- Gen.choose(-3.0, 3.0)
    x4 <- Gen.choose(-3.0, 3.0)
  } yield PhenotypeChange(Array(x1, x2, x3, x4))

  implicit lazy val arbPhenotypeChange: Arbitrary[PhenotypeChange] = Arbitrary(genPhenotypeChange)

  lazy val genAllelle: Gen[Allelle] = for {
    e  <- arbitrary[PhenotypeChange]
    de <- arbitrary[PhenotypeChange]

  } yield Allelle(e, de)

  implicit lazy val arbAllelle: Arbitrary[Allelle] = Arbitrary(genAllelle)

  lazy val genDnaString: Gen[DnaString] = for {
    a1  <- arbitrary[Allelle]
    a2  <- arbitrary[Allelle]
    a3  <- arbitrary[Allelle]
    a4  <- arbitrary[Allelle]
    a5  <- arbitrary[Allelle]

  } yield DnaString (Array(a1, a2, a3, a4, a5))

  implicit lazy val arbDnaString: Arbitrary[DnaString] = Arbitrary(genDnaString)

  lazy val genIndividual: Gen[Individual] = for {
    sex <- arbitrary[Sex]
    birthGeneration <- Gen.choose(0, 100)
    dna1 <- arbitrary[DnaString]
    dna2 <- arbitrary[DnaString]
    phenotype <- arbitrary[Phenotype]
  } yield Individual(sex, birthGeneration, (dna1, dna2), phenotype)

  implicit lazy val arbIndividual: Arbitrary[Individual] = Arbitrary(genIndividual)

  lazy val genPopulation: Gen[Population] = for {
    individuals <- arbitrary[Seq[Individual]]
    generation <- Gen.choose(0,100)
  } yield Population(generation, individuals)

  implicit lazy val arbPopulation: Arbitrary[Population] = Arbitrary(genPopulation)
}
