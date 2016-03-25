
import scala.util.Random

/**
  * https://programmingpraxis.com/2016/03/25/leftpad/
  *
  * This is pretty silly, so use the exercise to experiment with
  * some simple benchmarking...
  */
object LeftPad {

  case class Result(avgTimePerCall: Float, opsPerMs: Float)

  type Results = (Result, Result)

  /**
   * @return time in milliseconds that it took the run the function n times
   */
  def time(n: Int, f: => Unit): Long = {
    val start = System.currentTimeMillis()
    for(x <- 1 to n) { f }
    System.currentTimeMillis() - start
  }

  /**
   * Compare the timing of two functions
   */
  def compare(f1: => Unit, f2: => Unit, n: Int): Results = { 
    val f1Time = time(n, f1)
    val f2Time = time(n, f2)
    (Result(f1Time / n.toFloat, n.toFloat / f1Time), 
     Result(f2Time / n.toFloat, n.toFloat / f2Time))
  }

  /**
   * print a report
   */
  def report(results: Results): Unit = {
    val (f1result, f2result) = results
    println("f1: avg " + f1result.avgTimePerCall + "ms per call, " + f1result.opsPerMs + " ops/ms")
    println("f2: avg " + f2result.avgTimePerCall + "ms per call, "  + f2result.opsPerMs + " ops/ms")
    println("f2 is " + f2result.opsPerMs / f1result.opsPerMs + " times as fast as f1")
  }

  /**
   * This version creates the space padding in one shot, and also
   * prepends in one shot.
   */
  def leftPad(s: String, n: Int) = " " * (n - s.length) + s

  /**
   * This slow version prepends a space to (a new copy of) the string
   * on each iteration. yuck.
   */
  def leftPadSlow(s: String, n: Int) = {
    var s2 = s
    (0 until (n - s.length)).foreach((x) => { s2 = " " + s2 })
    s2
  }

  def main(args: Array[String]): Unit = {
    assert(leftPad("hello", 10) == "     hello")
    assert(leftPadSlow("hello", 10) == "     hello")

    // double our string padding length each time: from this, we can
    // easily tell that the run time for the fast version doesn't
    // quite double (~ linear time), but that run time for slow
    // version more than doubles (~ quadratic)

    List(100, 200, 400, 800).foreach(strLen => {
      val n = 100000
      val baseStringLength = 50

      println("Running each function " + n + " times, padding strings to length=" + strLen)
      
      def fastF = leftPad(Random.nextString(baseStringLength), strLen)
      def slowF = leftPadSlow(Random.nextString(baseStringLength), strLen)

      // discard 1st set of results, which will be distorted by JIT optimization
      compare(slowF, fastF, n)

      val results = compare(slowF, fastF, n)
      report(results)
    })
  }

}
