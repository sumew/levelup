package trees

class CountComponents {

  def inOrder(adjList: Map[Int,Set[Int]],from: Int): Set[Int] = {
    def loop(stack: List[Int], output: Set[Int]): Set[Int] = stack match {
      case Nil => output
      case x::xs => loop((adjList.get(x).getOrElse(Set.empty) -- output).toList:::xs, output + x)
    }
    loop(from::Nil,Set.empty)
  }


  def countComponents(n: Int, edges: Array[Array[Int]]): Int = {
    val adjList = edges.foldLeft(Map.empty[Int,Set[Int]]){(adj,arr) =>
      val (first,second) = ( arr(0),arr(1) )
      adj.updated(first, adj.get(first).getOrElse(Set.empty) + second).updated(second,adj.get(second).getOrElse(Set.empty) + first)
    }

    val (c, _) = (0 until n).foldLeft((0,Set.empty[Int])){case ((count,visited),vertex) =>
      (if(visited.contains(vertex)) count else count+1,visited ++ inOrder(adjList,vertex))

    }
    c

  }


  def countComponents2(n: Int, edges: Array[Array[Int]]): Int = {
    def adjacencyList[T](array: Array[Array[T]]): Map[T,Set[T]] = {
      array.foldLeft(Map.empty[T,Set[T]]){case (acc,arr) =>
        acc.updated(arr(0),acc.get(arr(0)).getOrElse(Set.empty[T]) + arr(1))
          .updated(arr(1), acc.get(arr(1)).getOrElse(Set.empty[T]) + arr(0))
      }

    }

    def dfp(root: Int, adj: Map[Int,Set[Int]]): Set[Int] = {
      def loop(stack: List[Int], output: Set[Int]): Set[Int] = stack match {
        case Nil => output
        case t::ts => loop(
          adj.get(t).fold(Set.empty[Int])(_ -- output).toList:::ts,
          output + t
        )
      }
      loop(root::Nil, Set.empty)
    }

    val adjMap = adjacencyList[Int](edges)
    (0 until n).foldLeft((0,Set.empty[Int])){case ((count,visited),node) =>
      if(!visited.contains(node))
        (count+1 ,visited ++ dfp(node,adjMap))
      else
        (count,visited)
    }._1


  }
}
