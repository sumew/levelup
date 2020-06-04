def solution(a: Array[Int]): Int = {
  // write your code in Scala 2.12
  val totalSum: Int = a.reduce(_ + _)
  val init = 0
  a.foldLeft((totalSum, init)) {
    case ((currentMin, agg), idx) => {
      (math.min(currentMin, math.abs((agg + idx) - (totalSum - (agg + idx)))), agg + idx)
    }
  }
}._1

solution(Array(3, 1, 2, 4, 3))
