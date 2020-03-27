package object string_manipulation {

  def reverse(x: Int): Int = {

    def loop(input: Int, acc: Int, power: Int): Int = {

      val runningTotal = (acc  + (Math.pow(10, power)* (input % 10)))

      if(runningTotal.abs * -1 < Int.MinValue || runningTotal.abs > Int.MaxValue)
        0
      else if(input.abs < 10)
        acc + input
      else
        loop(input / 10, runningTotal.toInt, power-1)
    }

    if(Int.MinValue <= x) 0 else loop(x,0,Math.log10(x.abs).toInt)
  }

}
