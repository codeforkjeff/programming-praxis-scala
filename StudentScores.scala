
import java.io.File
import scala.collection.immutable.HashMap
import scala.io.Source

/**
  * https://programmingpraxis.com/2016/04/05/java-interview-question/
  */
object StudentScores {

  class StudentScore(val student: Int, val score: Int)

  def splitLine(line: String): (String, StudentScore) = {
    val pieces = line.split("\\|")
    val course = pieces(1)
    val studentScore = new StudentScore(pieces(0).toInt, pieces(2).toInt)
    (course, studentScore)
  }

  def scoresOfStudentsWithLowestIds(filename: String): Map[String, Int] = {
    val source = Source.fromFile(new File(filename))
    val initialMap = HashMap[String, StudentScore]()
    val coursesToStudentScores =
      source.getLines().foldLeft(initialMap)((map, line) => {
        val (course, studentScore) = splitLine(line)
        if (!map.contains(course) || studentScore.student < map(course).student) {
          map + (course -> studentScore)
        } else {
          map
        }
      })
    // transform values in Map from StudentScores to Int scores
    coursesToStudentScores.map(tuple => tuple._1 -> tuple._2.score)
  }

  def report(filename: String): String = scoresOfStudentsWithLowestIds(filename).map(tuple => {
    val (course, score) = tuple
    course + " " + score.toString
  }).mkString("\n")

  def main(args: Array[String]): Unit = {
    val results = report("StudentScores.txt").split("\n")
    assert(results.contains("Math 45"))
    assert(results.contains("English 81"))
  }

}
