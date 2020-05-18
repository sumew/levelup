import scala.collection.immutable.Queue

def updateMatrix(matrix: Array[Array[Int]]): Array[Array[Int]] = {
  val rows = matrix.length
  val cols = if(rows == 0) 0 else matrix(0).length
  val validNeighbors: PartialFunction[(Int,Int), Boolean] = {
    case (row: Int, col: Int) =>
      if (col < 0 || row < 0 || row >= matrix.length || col >= matrix(0).length) false else true
  }

      def neighbors(row: Int, col: Int): Array[(Int,Int)] =
        Array((row-1,col),(row+1,col),(row, col-1),(row,col+1)).filter(validNeighbors)

      def bfs(point: (Int,Int)): Int = {
        def loop(q: Queue[((Int,Int),Int)], visited: Set[(Int,Int)]): Int = q.dequeueOption match {
          case None => -1
          case Some((((row,col),dis), rest)) => {
            val n = neighbors(row,col).filterNot(visited.contains).map(p => (p,dis+1))
            n.find{ case ((r,c),_) => matrix(r)(c) == 0} match {
              case None => loop(rest.enqueue(n.toIndexedSeq), visited ++ n.map(_._1))
              case Some ((_,d)) => d
            }
          }
        }

        loop(Queue((point,0)), Set.empty)
      }

      for(row <- ( 0 until rows); col <- 0 until cols if matrix(row)(col) == 1 ){
        matrix(row)(col)=bfs((row,col))
      }

  matrix
  }

updateMatrix(Array(Array(1)))



