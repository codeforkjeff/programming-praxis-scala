
/**
  * https://programmingpraxis.com/2016/03/04/wave-sorting/
  */
object WaveSort {

  /**
    * TODO: this is highly inefficient
    */
  def waveSort(ints : List[Int]) : List[Int] = {
    val allSorted = ints.sorted
    val halfReversed = allSorted.takeRight(allSorted.size / 2).reverse
    val isOdd = allSorted.size % 2 == 1
    val halfSorted = allSorted.take(allSorted.size / 2)
    val pairs = halfReversed.zip(halfSorted)
    val listOfLists = pairs.map(item => { List(item._1, item._2) })
    listOfLists.flatten ::: (if(isOdd) List(allSorted(allSorted.size / 2)) else Nil)
  }

  def main(args: Array[String]): Unit = {
    assert(waveSort(List()) == List())
    assert(waveSort(List(1)) == List(1))
    assert(waveSort(List(1,2,3,4,5,6)) == List(6, 1, 5, 2, 4, 3))
    assert(waveSort(List(1,2,3,4,5,6,7)) == List(7, 1, 6, 2, 5, 3, 4))
  }

}
