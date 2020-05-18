import scala.collection.immutable.Queue

/** *
  * Equations are given in the format A / B = k, where A and B are variables represented as strings, and k is a real number (floating point number).
  * Given some queries, return the answers. If the answer does not exist, return -1.0.
  *
  * Example:
  * Given a / b = 2.0, b / c = 3.0.
  * queries are: a / c = ?, b / a = ?, a / e = ?, a / a = ?, x / x = ? .
  * return [6.0, 0.5, -1.0, 1.0, -1.0 ].
  *
  * The input is: vector<pair<string, string>> equations, vector<double>& values, vector<pair<string, string>> queries , where equations.size() == values.size(), and the values are positive. This represents the equations. Return vector<double>.
  *
  * According to the example above:
  *
  * equations = [ ["a", "b"], ["b", "c"] ],
  * values = [2.0, 3.0],
  * queries = [ ["a", "c"], ["b", "a"], ["a", "e"], ["a", "a"], ["x", "x"] ].
  *
  *
  *
  * The input is always valid. You may assume that evaluating the queries will result in no division by zero and there is no contradiction.
  */


def calcEquation(equations: List[List[String]], values: Array[Double], queries: List[List[String]]): Array[Double] = {
  val adjMap: Map[String, Map[String, Double]] =
    equations.map(arr => (arr(0), arr(1))).zip(values)
      .foldLeft(equations.flatten.distinct.map(k => (k, Map((k -> 1D)))).toMap) {
        case (acc, ((n, d), r)) =>
          (acc + (n -> acc.get(n).fold(Map((d -> r)))(_ + (d -> r)))) +
            (d -> acc.get(d).fold(Map((n -> Math.pow(r, -1))))(_ + (n -> Math.pow(r, -1))))
      }

  def bfs(root: String, adjMap: Map[String,Map[String,Double]]): Queue[(String,Double)] = {
    def loop(queue: Queue[(String,Double)], output: Queue[(String,Double)], visited: Set[String]): Queue[(String,Double)] = queue.dequeueOption match {
      case None => output
      case Some((t, rest)) =>
        if(visited.contains(t._1)) loop(rest,output,visited)
        else loop(rest ++ adjMap.getOrElse(t._1, Queue()).map {
          case (k,v) => (k,v*t._2)}, output.enqueue(t), visited + t._1)
    }

    loop(Queue((root,1)), Queue.empty, Set.empty)
  }



  val queryPairs = queries.map(arr => (arr(0),arr(1)))
  val solutionMap = queryPairs.map{

    case (x,_) => adjMap.get(x).fold((x,Queue((x,-1D))))( _ => (x, bfs(x,adjMap)))

  }.toMap

  queryPairs.map{ case (x,y) =>
    solutionMap.get(x).fold(-1D)(q => q.find{ case (a,b) => a == y}.map(_._2).getOrElse(-1D))
  }.toArray

}


val equations = List(
  List("a", "b"), List("b", "c")
)

val values = Array(
  2.0, 3.0
)

val queries =
List(
  List("a","c"),List("b","a"),List("a","e"),List("a","a"),List("x","x"), List("b","c")
)
calcEquation(equations, values, queries)