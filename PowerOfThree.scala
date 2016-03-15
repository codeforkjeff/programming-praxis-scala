
/**
  * https://programmingpraxis.com/2016/03/01/powers-of-3/
  */
object PowerOfThree {

  def isPowerOfThree(n : Int, power : Int = 0, powerOfThree : Double = 1) : Boolean = {
    if (n < powerOfThree) {
      return false
    }
    if (n == powerOfThree) {
      return true
    }
    isPowerOfThree(n, power + 1, powerOfThree * 3)
  }

  def test(n : Int) : Boolean = {
    val result = isPowerOfThree(n)
    println(n + " is" + (if (result) "" else " NOT") + " a power of 3")
    result
  }

  def main(args: Array[String]) : Unit = {
    assert(test(1))
    assert(test(3))
    assert(!test(4))
    assert(!test(5))
    assert(test(27))
    assert(!test(28))
    assert(!test(1000))
  }
}
