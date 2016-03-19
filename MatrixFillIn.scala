
/**
  * https://programmingpraxis.com/2016/01/12/matrix-fill-in/
  */
object MatrixFillIn {

  type MatrixArray = Array[Array[Boolean]]

  // encapsulates a List representing a rows x cols sized matrix,
  // so that it can be processed more functionally
  class MatrixList(val matrix: List[Boolean], val rows: Int, val cols: Int) {

    def this(array: MatrixArray) {
      this(array.foldLeft(List[Boolean]())((result, item) => result ::: item.toList), array.length, array(0).length)
    }

    def asArray: MatrixArray = matrix.grouped(rows).map(_.toArray).toArray
  }

  def filledIn(matrixArray: MatrixArray): MatrixArray = {
    /**
      * @return list of tuples representing coordinates in matrix that are True
      */
    def analyze(matrix: MatrixList): List[(Int, Int)] = {
      val rows = matrix.rows
      val cols = matrix.cols
      def _analyze(matrix: List[Boolean], row: Int = 0, col: Int = 0): List[(Int, Int)] = {
        val coord = if (matrix.head) List((row, col)) else Nil
        if (row + 1 < rows) {
          coord ::: _analyze(matrix.tail, row + 1, col)
        } else {
          if (col + 1 < cols) {
            coord ::: _analyze(matrix.tail, 0, col + 1)
          } else {
            coord
          }
        }
      }
      _analyze(matrix.matrix)
    }

    /**
      * @param coords
      * @return 2-item tuple of Sets of rows and columns to fill
      */
    def rowsAndColsToFill(coords: List[(Int, Int)]): (Set[Int], Set[Int]) = {
      val rows = coords.map(_._1).distinct.toSet
      val cols = coords.map(_._2).distinct.toSet
      (rows, cols)
    }

    def fill(matrix: MatrixList, fillRows: Set[Int], fillCols: Set[Int]): MatrixList = {
      val rows = matrix.rows
      val cols = matrix.cols
      def _fill(matrix: List[Boolean], fillRows: Set[Int], fillCols: Set[Int], row: Int = 0, col: Int = 0): List[Boolean] = {
        val coord = fillRows.contains(row) || fillCols.contains(col) || matrix.head
        if(row + 1 < rows) {
          coord :: _fill(matrix.tail, fillRows, fillCols, row + 1, col)
        } else {
          if (col + 1 < cols) {
            coord :: _fill(matrix.tail, fillRows, fillCols, 0, col + 1)
          } else {
            List(coord)
          }
        }
      }
      new MatrixList(_fill(matrix.matrix, fillRows, fillCols), matrix.rows, matrix.cols)
    }

    val matrix = new MatrixList(matrixArray)

    val (fillRows, fillCols) = rowsAndColsToFill(analyze(matrix))

    fill(matrix, fillRows, fillCols).asArray
  }

  def dump(matrix: MatrixArray) = matrix.foreach((row) => println(row.map(if (_) 1 else 0).mkString(" ")))

  def main(args: Array[String]): Unit = {
    val x = Array.ofDim[Boolean](10, 10)
    x(0)(1) = true
    x(3)(5) = true
    x(3)(6) = true
    x(8)(8) = true
    println("original matrix:")
    dump(x)
    println("filled in matrix:")
    dump(filledIn(x))
  }
}
