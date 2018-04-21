package cz.nekola.frozenbeagle

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


object Naturalists {

  def notes(naturalists: Seq[Naturalist])(population: Population): Map[String, Double] = {
    naturalists.map(naturalist => naturalist.observe(population))
      .foldLeft(Map[String, Double]())(
        (a: Map[String, Double], b: Map[String, Double]) => {
          assert(a.keySet.intersect(b.keySet).isEmpty, "Two naturalists should not share a key " + a.keySet.intersect(b.keySet))
          a ++ b
        }
      )
  }
}

