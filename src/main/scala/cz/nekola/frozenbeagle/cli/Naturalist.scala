package cz.nekola.frozenbeagle.cli

import cz.nekola.frozenbeagle.Population

trait Naturalist {
  def observe(population: Population): Map[String, Double]
}

object Demograph extends Naturalist {
  override def observe(population: Population): Map[String, Double] = {
    Map( "generation" -> population.generation
       , "populationSize" -> population.size
       , "maleCount" -> population.males.size
       , "femaleCount" -> population.females.size
    )
  }
}


