
/**
  * https://programmingpraxis.com/2016/03/11/two-boys-and-a-canoe/
  */
object Canoes {

  val MAX_WEIGHT = 150

  // returns a List of Lists representing canoes
  def makeCanoes(weights: List[Int]): List[List[Int]] = {
    def _makeCanoes(weights: List[Int]): List[List[Int]] = {
      if (weights == Nil) {
        return Nil
      }
      val firstPerson = weights.last
      val remainderWithoutHeaviest = weights.dropRight(1)
      val secondPerson = if (remainderWithoutHeaviest.nonEmpty) remainderWithoutHeaviest.head else 0
      val canoe = if (secondPerson > 0 && firstPerson + secondPerson <= MAX_WEIGHT)
        List(firstPerson, secondPerson)
      else
        List(firstPerson)
      val remainder = if (secondPerson > 0) remainderWithoutHeaviest.tail else remainderWithoutHeaviest
      canoe :: makeCanoes(remainder)
    }
    _makeCanoes(weights.filter(_ <= MAX_WEIGHT).sorted)
  }

  def main(args: Array[String]) : Unit = {
    val weights = List(40, 120, 122, 124, 156, 100, 140, 20, 50, 80, 30, 60, 80, 100, 40, 70, 30, 20)
    val canoes = makeCanoes(weights)

    assert(canoes.size == 9)

    println(canoes.size + " canoes needed for " + weights.size + " people, as follows:")
    canoes.foreach(b => { println(b.mkString(" ")) })
  }

}