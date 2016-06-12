/**
  * Created by jeff on 4/21/16.
  */

/**
  * https://programmingpraxis.com/2016/04/08/google-interview-question/
  */
object MaxProductStrings {

  type Pair = (String, String)

  def haveLettersInCommon(s1: String, s2: String): Boolean =
    s1.exists(s2.indexOf(_) != -1)

  // returns pairs of words in strings that do not share a letter
  def combos(strings: List[String]): List[Pair] =
    if (strings.size > 1) {
      val head = strings.head
      strings.tail.filter(!haveLettersInCommon(head, _)).map((head, _)) ::: combos(strings.tail)
    } else {
      Nil
    }

  def maxProductStrings(strings: List[String]): Int =
    combos(strings).foldLeft(0)((maxProd, pair) => {
      val prod = pair._1.length * pair._2.length
      if (prod > maxProd) prod else maxProd
    })

  def main(args: Array[String]): Unit = {
    val words = List("ABCW", "BAZ", "FOO", "BAR", "XTFN", "ABCDEF")
    assert(maxProductStrings(words) == 16)
  }
}