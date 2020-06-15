import scala.reflect.ClassTag

type Matrix[T] = Array[Array[T]]

def Matrix[T: ClassTag](rows: Int, cols: Int, value: T) =
  Array.fill[T](rows, cols)(value)

case class State(minimum: Int, k: Int) {
  override def toString = s"$minimum:$k"
}

def printMatrix[T](matrix: Matrix[T]) = {
  (0 until matrix.length).foreach(i => println(matrix(i).mkString(" ")))
}

def bottomUp(p: Array[Int]): Matrix[State] = {

  val n = p.length - 1

  def minProducts(): Matrix[State] = {

    val m = Matrix[State](n, n, State(Int.MaxValue, -1))

    (0 until n).foreach { idx => m(idx)(idx) = State(minimum = 0, k = 0) }

    for (l <- 2 to n)
      for (a <- 0 to n - l) {
        val b = a + l - 1
        for (k <- a until b) {
          val mab =
            m(a)(k).minimum + m(k + 1)(b).minimum + p(a) * p(k + 1) * p(b + 1)
          if (mab < m(a)(b).minimum)
            m(a)(b) = State(mab, k)
        }
      }

    m
  }

  minProducts()

}

def parenthesize(input: Array[Int]): String = {

  val n = input.length - 1
  val m = bottomUp(input)

  def loop(begin: Int, end: Int): String = ((end - begin), m(begin)(end).k) match {
    case (0, _) => s"A${begin}" //{s"(${input(begin)} * ${input(begin + 1)})"
    case (_, k) => s"(${loop(begin, k)}${loop(k + 1, end)})" // s"(${loop(begin, m(begin)(end).k)} * ${loop(m(begin)(end).k + 1, end)})"
  }
  loop(0, n - 1)

}

val p = Array(30, 35, 15, 5, 10, 20, 25)
parenthesize(p)

val pp = Array(5, 10, 3, 12, 5, 50, 6)

parenthesize(pp)
