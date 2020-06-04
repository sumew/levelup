def cutRod(prices: Array[Int], size: Int): Int = {
  if (size == 0) 0
  else {
    var max = prices(size - 1)
    (0 until size).foreach { index =>
      max = math.max(max, prices(index) + cutRod(prices, size - (index + 1)))
    }
    max
  }

}

//cutRod(Array(1, 5, 8, 9, 10), 4)

def bottomUp(prices: Array[Int], size: Int): Int = {
  println(s"prices: ${prices.mkString(",")}")
  val p = (1 to size).foldLeft(Array.fill(size)(0)) { (map, index) =>
    println(s"index: $index, map($index): ${map(index)}")
    map.updated(
      index - 1,
      (1 until index).foldLeft(map(index - 1)) { (max, j) =>
        {
          println(s"max: $max, j: $j, map(${index - j}): ${map(index - j)}")
          math.max(max, prices(j - 1) + bottomUp(prices, map(index - j)))
        }
      }
    )

  }
  println(s"p: ${p.mkString(",")}")
  p(size)
}

//bottomUp(Array(1, 5, 8, 9, 10), 4)

def bottomUp2(prices: Array[Int], size: Int): Int = {
  val map = Array.fill[Int](size)(0)
  for (i <- (1 to size)) {
    var q = Int.MinValue
    for (j <- (1 to i)) {
      println(
        s"i: $i, j: $j, max: ${math.max(q, prices(j - 1) + map(i - j - 1))}"
      )
      q = math.max(q, prices(j - 1) + map((i - j)))
    }
    map(i - 1) = q
  }
  println(s"map: ${map.mkString(",")}")
  map(size - 1)
}

bottomUp2(Array(1, 5, 8, 9, 10), 4)
