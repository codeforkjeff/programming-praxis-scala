/**
  * https://programmingpraxis.com/2016/06/10/linear-regression/
  */
object LineOfBestFit {

  class Point(val x: Double, val y: Double)
  class Line(val slope: Double, val intercept : Double) {
    def calculateY(x : Double) = this.slope * x + this.intercept
  }

  def sum(p : List[Point], pt : Point => Double) = p.foldLeft(0.0)((a, b) => a + pt(b))

  def sumX(p : List[Point]) = sum(p, _.x)

  def sumY(p : List[Point]) = sum(p, _.y)

  def sumXY(p : List[Point]) = sum(p, pt => pt.x * pt.y)

  def sumXSqr(p : List[Point]) = sum(p, pt => Math.pow(pt.x, 2))

  /**
    * Calculate linear regression, returning a line of best fit for
    * a set of x, y coordinates
    */
  def bestFit(p : List[Point]) : Line = {
    val n = p.size
    val sumx = sumX(p)
    val slope = (n * sumXY(p) - sumX(p) * sumY(p)) / (n * sumXSqr(p) - (sumx * sumx))
    val intercept = (sumY(p) - slope * sumx) / n
    new Line(slope, intercept)
  }

  def main(args: Array[String]) : Unit = {
    val points = List(new Point(60, 3.1),
      new Point(61, 3.6),
      new Point(62, 3.8),
      new Point(63, 4.0),
      new Point(65, 4.1))
    val bf = bestFit(points)
    assert(bf.slope.toString == "0.18783783783783292")
    assert(bf.intercept.toString == "-7.963513513513208")
    assert(bf.calculateY(64).toString == "4.058108108108099")
  }

}