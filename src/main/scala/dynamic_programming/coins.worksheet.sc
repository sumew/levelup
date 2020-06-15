def coinChange(coins: Array[Int], amount: Int): Int = {

  var cache = Array.fill[Option[Int]](amount)(None)

  def loop(amt: Int, count: Int): Int = amt match {
    case 0          => count
    case n if n < 0 => -1
    case n => {
      var min = Int.MaxValue
      for (coin <- coins) {
        val m = loop(n - coin, count + 1)
        println(s"loop(${n - coin}) == $m, min: $min")
        if (min > m && m >= 0) min = m
      }
      if (min == Int.MaxValue) -1 else min
    }
  }

  loop(amount, 0)

}

coinChange(Array(1, 3, 5, 10), 11)

def coinChangeM(coins: Array[Int], amount: Int): Int = {

  val cache = scala.collection.mutable.Map.empty[Int, Int]

  def loop(amt: Int): Int = {
    cache.get(amt) match {
      case Some(s) => s
      case None =>
        amt match {
          case 0          => 0
          case n if n < 0 => -1
          case n => {
            var min = Int.MaxValue
            for (coin <- coins) {
              val m = loop(amt - coin)
              if (m >= 0 && m < min) min = m + 1
            }
            val count = if (min == Int.MaxValue) -1 else min
            cache.update(n, count)
            count
          }

        }
    }
  }
  if (amount == 0) 0
  else {
    val result = loop(amount)
    cache(amount)
  }

}

class MinCoins extends dp.DP[Int, Int, Int, Int] {

  override def key(state: Int) = state
  override def better(current: Int, ultima: Int) = if (current > 0 && current-1 < ultima) current else ultima
  override def errorCheck(value: Int) = if (value < 0) -1 else value
  override val errorCase = s => s < 0
  override val baseCase = s => s == 0

  override def breakDown(state: Int, choice: Int): Int = state - choice

  override def update(currentVal: Int, currentCoin: Int): Int = currentVal + 1

  override val baseValue = 0
  override val errorValue = -1
  override val ultima = Int.MaxValue

}

val mc = new MinCoins
mc.run(19, Array(1, 2, 5))

coinChangeM(Array(1), 0)
coinChangeM(Array(1, 2, 5), 11)
coinChangeM(Array(1, 5, 10), 100)



