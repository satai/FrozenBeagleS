package cz.nekola.frozenbeagle

import java.lang.Math.sqrt

import cz.nekola.frozenbeagle.SimulationConstants.zeroPhenotypeVec

case class Phenotype(components: List[Double]) {
  def distance(that: Phenotype) =
      sqrt( (this.components, that.components)
             .zipped
             .map((x, y) => x - y)
             .map((x) => x * x)
             .sum)

  override def toString = this.components.mkString("(", ", ", ")")
}

object Phenotype {
  val zeroPhenotype = Phenotype (zeroPhenotypeVec)
}