
/**
  * https://programmingpraxis.com/2016/02/23/arithmetic-sequence/
  */
object ArithmeticSequence {

  def isArithmeticSequence(ints : List[Int]) : Boolean = {
    def _isArithmeticSequence(ints : List[Int]) : Boolean = {
      if (ints.size <= 1) {
        return true
      }
      val withoutFirst = ints.tail
      val diffs = withoutFirst.zip(ints).map(pair => pair._1 - pair._2)
      val firstDiff = diffs.head
      diffs.forall(n => n == firstDiff)
    }
    _isArithmeticSequence(ints.sorted)
  }

  def main(args: Array[String]): Unit = {
    assert(isArithmeticSequence(List(2,9,16)))
    assert(isArithmeticSequence(List(2,16,9)))
    assert(isArithmeticSequence(List(1,3,5,7)))
    assert(isArithmeticSequence(List(5,3,1,7)))
    assert(!isArithmeticSequence(List(5,4,1,7)))
  }

}