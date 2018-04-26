package cz.nekola.frozenbeagle.cli

case class Result( simulations: Seq[Seq[Map[String, Double]]]
                 , params: Params
                 , runtime: Double
                 , startTime: String
                 )
