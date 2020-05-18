

val input = Array("11110".toCharArray, "11010".toCharArray, "11000".toCharArray, "00000".toCharArray)

input(0).length


case class Point(i: Int, j: Int) {

  def neighbors(d: Point): List[Point] =
    List(Point(i - 1, j), Point(i + 1, j), Point(i, j - 1), Point(i, j + 1))
      .filter {
        case Point(-1, _) => false
        case Point(d.i, _) => false
        case Point(_, -1) => false
        case Point(_, d.j) => false
        case _ => true
      }
}
object Point {
  def toIndex(p: Point, d: Point): Int = (d.j * p.i) + p.j
  def toPoint(idx: Int, d: Point): Point = Point(idx / d.j, idx - ((idx / d.j) * d.j))
  def valueAt(p: Point, grid: Array[Array[Char]]): Char = grid(p.i)(p.j)
  def valueAt(idx: Int, d: Point, grid: Array[Array[Char]]): Char = valueAt(toPoint(idx,d), grid)
}


val id: Point => Array[(Int,Int)] = d => (0 until d.i * d.j).toArray.zip(Array.fill(d.j * d.i)(1))

def root(id: Array[(Int, Int)], idx: Int): (Int, Int) = {
  def loop(p: Int): (Int, Int) =
    if (id(p)._1 == p) id(p)
    else {
      id(p) = id(id(p)._1) //Path compression
      loop(id(p)._1)
    }

  loop(idx)

}

def connected(id: Array[(Int, Int)], p: Int, q: Int) =
  root(id, p) == root(id, q)
def union(id: Array[(Int, Int)], p: Int, q: Int) =
  (root(id, p), root(id, q)) match {
    case ((rp, _), (rq, _)) if rp == rq => id
    case ((rp, sp), (rq, sq)) if sp > sq => {
      id.update(rq, (rp, (sp + sq)))
      id.updated(rp, (rp, sp + sq))
    }
    case ((rp, sp), (rq, sq)) =>
      {
        id.update(rp, (rq, sp + sq))
        id.updated(rq, (rq, sp + sq))
      }
  }



/**
  * 11110
  * 11010
  * 11000
  * 00000
  *
  * @param grid
  * @return
  */
def numIslands(grid: Array[Array[Char]]): Int = {
  val d = Point(grid.length,if(grid.length == 0) 0 else grid(0).length)
  val unioned = (0 until(d.i * d.j, 2)).filter(idx => Point.valueAt(idx,d,grid).equals('1'))
    .foldLeft(id(d)){case (acc,idx) =>
    Point.toPoint(idx,d).neighbors(d).filter(p => grid(p.i)(p.j) == '1')
        .foldLeft(acc){case (neighborhood,neighbor) => union(neighborhood, idx, Point.toIndex(neighbor,d))}
    }

  unioned.unzip._1.map(root(unioned,_)).filter{case (r,_) => {
    val Point(i,j) = Point.toPoint(r,d)
    grid(i)(j) == '1'
  }}.unzip._1.distinct.size

}




