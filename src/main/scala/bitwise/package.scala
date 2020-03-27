package object bitwise {

  import scala.annotation.tailrec

  def countBits(x: Int): Int = {
    @tailrec
    def loop(input: Int, current: Int): Int = input match {
      case 0 => current
      case _ => loop((input >>> 1),current + (input & 1) )
    }
    loop(x,0)
  }

  def parity(x: Int): Int = {
    @tailrec
    def loop(x: Int, parity: Int): Int = x match {
      case 0 => parity
      case _ => loop((x >>> 1), parity ^ (x & 1))
    }
    loop(x,0)
  }

  def lowestBit(x: Int): Int = {
    def loop(input: Int, acc: Int): Int = input match {
      case 0 => acc
      case _ => loop(input >> 1, acc + 1)
    }
    loop((x & -x), 0)
  }

  def isEven(n: Int) = (n & 1) != 1

  //def reverse(n: Int): Int = 0 to 31 foldLeft(n)((acc: Int,_: Int) => if((acc & 1)==1) (acc >>> 1) | Int.MinValue else (acc >>> 1))

  def lowestSetBit(n: Int): Int = {
    def loop(n: Int, acc: Int): Int = n match {
      case 0 => acc
      case _ => loop((n>>>1), acc + 1)
    }

    loop(n & ~(n-1), 0)
  }


  def reverse2(n: Int): Int = {
    def loop(n: Int, acc: Int): Int = n match {
      case 0 => acc
      case _ => loop((n >>> 1), (acc << 1) |  (n & 1))
    }
    (loop(n,0))
  }
}
