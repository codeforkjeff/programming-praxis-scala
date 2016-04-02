
/**
  * https://programmingpraxis.com/2016/04/01/sum-and-xor/
  *
  * The PP solution is a WAY more optimal single-pass solution.
  * The lesson to be learned here is that if there are multiple criteria
  * to be applied on a dataset, see if you can avoid multiple passes by
  * combining them algebraically into a single equation.
  *
  * At least the solution below is more readable. :P
  */
object SumAndXor {
  type Pair = (Int, Int)

  // all unique (ie ignore order) pairs of positive integers whose sum is s
  def sumUniquePairs(s: Int): Seq[Pair] = {
    (1 to s/2).map(i => (i, s - i))
  }

  def sumAndXor(s: Int, x: Int): List[Pair] = {
    val results = sumUniquePairs(s).filter(pair => (pair._1 ^ pair._2) == x)
    // if (a, b) is a result, so is (b, a)
    (results ++ results.map(pair => (pair._2, pair._1))).toList
  }

  def main(args: Array[String]): Unit = {
    assert(sumAndXor(9, 5) == List((2,7), (3,6), (7,2), (6,3)))
  }
}
