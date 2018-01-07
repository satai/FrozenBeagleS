package cz.nekola.frozenbeagle

case class Schema(elements: List[Option[Allelle]]) {
  override def toString: String = this.elements.map{_.getOrElse("*")}.mkString("<", "", ">")
}
