import scala.annotation.tailrec
import scala.collection.immutable.ListMap

trait Heap[T] {
  val store: Array[T]
  val compare: (T,T) => T
  def ultima: T
  def heapify: Heap[T]


}

def reverse(x: Int): Int = {

    def loop(input: Int, acc: Int, power: Int): Int = {
      val runningTotal = (acc  + (Math.pow(10, power)* (input % 10)))
      if(runningTotal.abs * -1 < Int.MinValue)
        0
     // println (s"runnintTotal: $runningTotal")
      if(runningTotal.abs > Int.MaxValue)
        0
      else if(input.abs < 10)
        acc + input
      else
        loop(input / 10, runningTotal.toInt, power-1)
    }

  if(Int.MinValue <= x) 0
  else
  loop(x,0,Math.log10(x.abs).toInt)
}



reverse(123)
//reverse2(-123)
 //Int.MaxValue
//Int.MinValue
//reverse(-2147483648)


def isPalindrome(x: Int): Boolean = {
  if(x < 0) false
  else {
   val reversed =  x.toString.reverse.toLong
    if(reversed > Int.MaxValue) false
    else reversed.toInt == x
  }
}

"abcd".toList

def romanToInt(s: String): Int =  {
  val mapping = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)
  val exceptions = Map(('I','V') -> 4, ('I','X') -> 9, ('X','L') -> 40, ('X','C') -> 90,
    ('C','D') -> 400, ('C','M') -> 900)

  def loop(l: List[Char], acc: Int): Int = l match {
    case Nil => acc
    case x::y::ys if(exceptions.contains((x,y))) => loop(ys,acc + exceptions((x,y)))
    case x::xs => loop(xs, acc + mapping(x))
  }

  loop(s.toList, 0)
}


romanToInt("XLIII")





val digits = Array(1,10,100,1000)

def split(n: Int): (Int,Int) = {
  val exp = Math.log10(n.abs).toInt
  (n/digits(exp) * digits(exp), n % digits(exp))
}
val listmap = ListMap(
  1000 -> "M",
  900 -> "CM",
  500 -> "D",
  400 -> "CD",
  100 -> "C",
  90 -> "XC",
  50 -> "L",
  40 -> "XL",
  10 -> "X",
  9 -> "IX",
  5 -> "V",
  4 -> "IV",
  1 -> "I"
)


split(3999)

def intToRoman(n: Int) : String = {

  @tailrec
  def loop(cur: Int, roman: String): String = cur match {
    case 0 => roman
    case n => split(n) match {
      case (main,rem) => {
        val (key, value) = listmap.filter { case (k,_) => k <= main }.head
        loop(main - key + rem, roman + value)
      }
    }

  }
  loop(n,"")
}

def common(a: String, b: String) : String = {
  a.zip(b).filter{ case (x,y) => x.equals(y)}.map(_._1).mkString

}

common("", "flow")

def longestCommonPrefix(strs: Array[String]): String = if(strs.size == 0) "" else


   strs.min.zip(strs.max).takeWhile{ case (x,y) => x.equals(y)}.map(_._1).mkString




    longestCommonPrefix(Array("flow", "fl"))

val strs = Array("abcdefg", "a", "zzddddrasdfasdfdfsa")
strs.max
strs.min

def hammingWeight( n: Int): Int = {

  def loop(cur: Int, count: Int): Int = cur match {
    case 0 => count
    case nz => loop(nz >>> 1, count + (nz & 1))
  }

  loop(n,0)
}


