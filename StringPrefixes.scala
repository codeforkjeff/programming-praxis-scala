
/**
  * https://programmingpraxis.com/2016/03/08/string-prefixes/
  */
object StringPrefixes {

  // returns prefix common to both strings, or empty string
  def getCommonPrefix(s1 : String, s2 : String, i : Int = 0) : String = {
    if(i < s1.size && i < s2.size && s1(i) == s2(i)) {
      return s1(i) + getCommonPrefix(s1, s2, i+1)
    }
    ""
  }

  // finds the common string prefix for a List of strings
  def findCommonPrefix(strings : List[String], prefix : String = "") : String = {
    // if there's no more strings to compare, return prefix
    if (strings.size == 0) {
      return prefix
    }
    // if there's no prefix yet, use the 1st string as the prefix
    if (prefix == "") {
      return findCommonPrefix(strings.tail, strings.head)
    }
    // calculate new prefix (if any) and do recursion
    val newPrefix = getCommonPrefix(strings.head, prefix)
    if (newPrefix != "") findCommonPrefix(strings.tail, newPrefix) else ""
  }

  def main(args: Array[String]) : Unit = {
    assert(findCommonPrefix(List("I love cats", "I love dogs")) == "I love ")
    assert(findCommonPrefix(List("I have a car", "I have taste")) == "I have ")
    assert(findCommonPrefix(List("I have a car", "I have taste", "I hate mice", "I hail Satan")) == "I ha")
    assert(findCommonPrefix(List("I have a car")) == "I have a car")
  }
}