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
    case _ => loop((x >>> 1), (parity ^ (x & 1)))
  }
  loop(x,0)
}

parity(6)

//countBits(255)