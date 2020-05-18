import scala.annotation.tailrec

def numIslands2(m: Int, n: Int, positions: Array[Array[Int]]): List[Int] = {

  case class Point(root: Int, size: Int, land: Boolean)
  val id = (0 until n * m).toArray.map(Point(_, 1, false))

  def toIndex(row: Int, col: Int): Int = (row * n) + col

  val outOfBounds: PartialFunction[(Int, Int), Boolean] = {
    case (-1, _) => true
    case (_, -1) => true
    case (r, c) if (r >= m || c >= n) => true
    case _ => false
  }

  def neighbors(row: Int, col: Int): List[Int] =
    List((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1))
      .filterNot(outOfBounds).map { case (r, c) => toIndex(r, c) }
      .filter(id(_).land)


  def addLand(row: Int, col: Int) = id(toIndex(row, col)) match {
    case Point(_, _, true) =>
    case Point(r, c, _) => id(toIndex(row, col)) = Point(r, c, true)
  }


  def root(idx: Int): Point = {
    var i = idx
    while (id(i).root != i) {
      id(i) = id(id(i).root)
      i = id(i).root
    }
    id(i)
  }

  def union(p: Int, q: Int) = (root(p), root(q)) match {
    case (Point(rp, sp, lp), Point(rq, sq, lq)) if sp >= sq => {
      id(rp) = Point(rp, sp + sq, lp)
      id(rq) = Point(rp, sp + sq, lq)
    }
    case (Point(rp, sp, lp), Point(rq, sq, lq)) => {
      id(rp) = Point(rq, sp + sq, lp)
      id(rq) = Point(rq, sp + sq, lq)
    }
  }


  positions.map(arr => (arr(0), arr(1)))
    .filterNot{ case (r,c) => id(toIndex(r,c)).land}
    .map { case (r, c) =>
    addLand(r, c)
    neighbors(r, c).foreach(neighbor => union(toIndex(r, c), neighbor))
    id.filter(_.land).map(p => root(p.root)).distinct.length
  }.toList

}

val positions = Array(
  Array(0, 0), Array(2, 0), Array(0, 1), Array(2, 1),
  Array(0, 2), Array(2, 2), Array(0, 1), Array(1, 2)
)
numIslands2(3, 3, positions)



