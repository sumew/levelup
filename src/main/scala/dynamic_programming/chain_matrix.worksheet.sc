import scala.reflect.ClassTag

type Matrix[T] = Array[Array[T]]

def Matrix[T: ClassTag](rows: Int, cols: Int, value: T) = Array.fill[T](rows, cols)(value)

case class State(minimum: Int, k: Int)

def bottomUp(input: Array[Matrix[Int]]): Matrix[State] = {

  val p = input.map(matrix => matrix.length).appended(input(input.length - 1)(0).length)
  val n = input.length

  def minProducts(): Matrix[State] = {

    val m = Matrix[State](n, n, State(Int.MaxValue, -1))

    (0 until input.length).foreach { idx => m(idx)(idx) = State(minimum = 0, k = 0) }

    for (l <- 2 to n)
      for (a <- 0 to n - l) {
        val b = a + l - 1
        for (k <- a until b) {
          val mab = m(a)(k).minimum + m(k + 1)(b).minimum + p(a) * p(k + 1) * p(b + 1)
          if (mab < m(a)(b).minimum)
            m(a)(b) = State(mab, k)
        }
      }

    m
  }

  minProducts()

}

val m = bottomUp(Array(Matrix(30, 35, 0), Matrix(35, 15, 0), Matrix(15, 5, 0), Matrix(5, 10, 0), Matrix(10, 20, 0), Matrix(20, 25, 0)))
val min = m(0)(5)

def parens(m: Matrix[State]): String = {

  def loop(range: Range): String = (m(range.start)(range.end).k, range.length) match {
    case (_, 0)                     => s"A${range.start}"
    case (k, _) if k == range.start => s"${Array.fill(range.length)("A").zip(range).map { case (a, b) => s"$a$b" }.mkString("")}"
    case (k, l) => {

      val (left, right) = range.splitAt(k)
      println(s"range: $range, length: $l, k: $k")

      s"(${loop(left)})(${loop(right)})"
    }

  }
  loop(0 to m.length - 1)

}

parens(m)
