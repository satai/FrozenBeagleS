package cz.nekola.frozenbeagle

import java.lang.Math.sqrt

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop


class PhenotypeSpec extends FunSpec with Matchers with Checkers {
    describe("Phenotype") {
        describe("Distance") {
            it("is euklidean one by example") {
                Phenotype(List(-1.0, 0.0, 0.0, 0.0)) distance Phenotype(List(0.0, 1.0, 0.0, 0.0)) should
                    equal(sqrt(2))
            }

            it("distance of a phenotype to itself is zero") {
                check {
                    (x1: Double, x2 : Double, x3: Double, x4 : Double) =>
                        val p1 = Phenotype(List(x1, x2, x3, x4))
                        0.0 == (p1 distance p1)
                }
            }
        }
    }
}
