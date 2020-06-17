def mincostTickets(days: Array[Int], costs: Array[Int]): Int = {

  val costMap = Map(1 -> costs(0), 7 -> costs(1), 30 -> costs(2))

  def loop(d: Array[Int], cost: Int): Int = d match {
    case arr if arr.isEmpty => cost
    case arr => {
      var min = Int.MaxValue
      for ((k, v) <- costMap) {
        val m = loop(d.dropWhile(_ < d(0) + k), cost + v)
        if (min > m) min = m
      }
      min
    }
  }

  loop(days, 0)

}

mincostTickets(Array(1, 4, 6, 7, 8, 20), Array(2, 7, 15))
mincostTickets(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31), Array(2, 7, 15))

def mincostTicketsM(days: Array[Int], costs: Array[Int]): Int = {

  val costMap = Map(1 -> costs(0), 7 -> costs(1), 30 -> costs(2))
  val cache = scala.collection.mutable.Map.empty[String, Int]

  def loop(d: Array[Int]): Int = cache.get(d.mkString(",")) match {
    case Some(s) => s
    case None =>
      d match {
        case arr if arr.isEmpty => 0
        case d => {
          var min = Int.MaxValue
          for ((k, v) <- costMap) {
            val m = loop(d.dropWhile(_ < d(0) + k)) + v
            if (min > m) min = m
          }
          cache.update(d.mkString(","), min)
          min
        }
      }
  }

  loop(days)

}
mincostTicketsM(Array(1, 4, 6, 7, 8, 20), Array(2, 7, 15))
mincostTicketsM(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31), Array(2, 7, 15))
mincostTicketsM(Array(), Array(2, 7, 15))


val minTix = new dp.DP[Array[Int], (Int,Int), String, Int] {
override def breakDown(state: Array[Int], choice: (Int, Int)) = state.dropWhile(_ < state(0) + choice._1)

  override def update(value: Int, current: (Int,Int)) = value + current._2

  override def key(state: Array[Int]) = state.mkString(",")

  override def better(current: Int, ultima: Int) = math.min(current, ultima)

  override def errorCheck(value: Int) = value

  override val errorCase = _ => false
  override val baseCase =  s => s.isEmpty
  override val baseValue = 0
  override val errorValue =  -1
  override val ultima = Int.MaxValue
}


minTix.run(Array(1, 4, 6, 7, 8, 20), Array(1,7,30).zip(Array(2, 7, 15)))

(Array(2, 7, 15).zip(Array(1,7,30))).mkString(",")
