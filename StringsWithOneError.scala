
/**
  * https://programmingpraxis.com/2016/01/29/compare-strings-with-one-error/
  */
object StringsWithOneError {

  /**
    * @param s1
    * @param s2
    * @return index of the error bet strings, -1 if the strings do not
    *         meet the criteria for having exactly one diff
    */
  def hasOneError(s1 : String, s2 : String) : Int = {

    // this version is inefficient b/c it creates lots of strings on each recursive iteration
    def _hasOneErrorInefficient(s1 : String, s2 : String, strictMode : Boolean = false, indexOfDiff : Int = 0) : Int = {
      // are we done?
      if (s2.isEmpty) {
        return indexOfDiff
      }
      if (s1.head == s2.head) {
        // if first char is the same, compare tails in the same mode, and advance indexOfDiff as necessary
        return _hasOneError(s1.tail, s2.tail, strictMode, if (strictMode) indexOfDiff else (indexOfDiff + 1))
      } else {
        // char did NOT match, so go into strict mode or return -1 if we're already in strict mode
        if (!strictMode) {
          return _hasOneError(s1, s2.tail, true, indexOfDiff)
        } else {
          return -1
        }
      }
    }

    /**
      * @param s1 a string
      * @param s2 should be the larger string by exactly 1 char
      * @param strictMode determines whether it's okay to encounter a first difference
      * @param i index of the s1 to compare to s2
      * @param indexOfDiff index of the difference found in s1
      * @return index of difference found, or -1 if strings don't have exactly one error as defined by exercise
      */
    def _hasOneError(s1 : String, s2 : String, strictMode : Boolean = false, i : Int = 0, indexOfDiff : Int = 0) : Int = {
      // are we done?
      if (i == s1.length) {
        indexOfDiff
      } else {
        val i2 = if (strictMode) i + 1 else i
        if (s1(i) == s2(i2)) {
          // if first char is the same, compare tails in the same mode, and advance indexOfDiff as necessary
          _hasOneError(s1, s2, strictMode, i + 1, if (strictMode) indexOfDiff else indexOfDiff + 1)
        } else {
          // char did NOT match, so go into strict mode or return -1 if we're already in strict mode
          if (!strictMode) {
            _hasOneError(s1, s2, true, i, indexOfDiff)
          } else {
            -1
          }
        }
      }
    }

    if (math.abs(s1.length - s2.length) == 1) {
      if (s1.length > s2.length) {
        return _hasOneError(s2, s1)
      } else {
        return _hasOneError(s1, s2)
      }
    }
    -1
  }

  def main(args: Array[String]): Unit = {
    assert(hasOneError("hello", "hallo") == -1)
    assert(hasOneError("hello", "blah") == -1)
    assert(hasOneError("hello", "heallo") == 2)
    assert(hasOneError("moral", "amoral") == 0)
    assert(hasOneError("moral", "aboral") == -1)
  }

}
