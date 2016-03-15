import java.io.File

import scala.collection.immutable.HashMap
import scala.io.Source

/**
  * https://programmingpraxis.com/2016/01/22/entropy/
  */
object ShannonEntropy {

  def shannonEntropy(f : File) : Float = {
    val initialMap : HashMap[Byte, Int] = HashMap()
    val size = f.length()
    val counts = Source.fromFile(f, "UTF-8").foldLeft(initialMap)((map, ch) => {
      val b = ch.toByte
      val oldVal = if (map.contains(b)) map(b) else 0
      map + (b -> (oldVal + 1))
    })
    val sum = counts.foldLeft(0f)((sum, pair) => {
      val freq : Float = pair._2.toFloat / size
      val log2 : Float = math.log(freq).toFloat / math.log(2).toFloat
      sum + (freq * log2)
    })
    sum * -1.0f
  }

  def main(args: Array[String]): Unit = {
    println(shannonEntropy(new File("ShannonEntropy.scala")))
  }
}