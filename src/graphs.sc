import scala.collection.immutable.{Queue, TreeMap}

/**
  * There are multiple ways to represent graphs, here are the most common ones
  * - Recursive
  * - Adjacency map
  * - Adjacency matrix
  */

/**
  *
  *               1
  *             / |  \
  *           /   |   \
  *         /     |    \
  *        2      3     8
  *      /  \    / \   / \
  *     4    5  6   7 9  10
  * Preorder: 1,2,4,5,3,6,7,8,9,10
  * InOrder: 4,2,5,1,6,3,7,9,8,10
  * PostOrder: 4,5,2,6,7,3,9,10,8,1
  *
  **/


case class Tree[T](value: T, children: List[Tree[T]])
case class BTree[T](value: T, left: Option[BTree[T]], right: Option[BTree[T]])


val tree = Tree(1,List(
  Tree(2,List(Tree(4,Nil), Tree(5,Nil))),
  Tree(3,List(Tree(6,Nil), Tree(7,Nil))),
  Tree(8,List(Tree(9,Nil), Tree(10,Nil)))
))

def adjList[T](tree: Tree[T])(implicit ordering: Ordering[T]): Map[T,List[T]] = {
  def loop(queue: Queue[Tree[T]], acc: Map[T,List[T]], visited: Set[T]): Map[T,List[T]] = queue match {
    case Queue() => acc
    case Tree(v,c) +:ts if(!visited.contains(v)) => loop(ts.enqueue(c),acc.updated(v, c.map(_.value)), visited + v)
  }
  loop(Queue(tree), TreeMap.empty[T,List[T]], Set.empty)
}

def preOrder[T,S](tree: Tree[T], f: T => S): Queue[S] = {
  def loop(g: Tree[T], output: Queue[S]): Queue[S] = g match {
    case Tree(v,c) =>   c.foldLeft(output.enqueue(f(v))){case (acc,n) => loop(n,acc)}
  }
  loop(tree,Queue.empty[S])
}

def postOrder[T,S](tree: Tree[T], f: T => S): Queue[S] = {
  def loop(g: Tree[T], output: Queue[S]): Queue[S] = g match {
    case Tree(v,rest) => rest.foldLeft(output){case (agg,node) => loop(node,agg)}.enqueue(f(v))
  }
  loop(tree,Queue.empty)
}

def inOrder[T,S](tree: Tree[T], f: T => S): Queue[S] = {
  def loop(g: Tree[T], output: Queue[S]): Queue[S] = g match {
    case Tree(v,l::ls) => ls.foldLeft(loop(l,output).enqueue(f(v))){case (acc,n) => loop(n,acc)}
    case Tree(v,Nil) => output.enqueue(f(v))
  }
  loop(tree, Queue.empty)
}

def preOrderR[T,S](tree: Tree[T], f: T => S): Queue[S] = {
  def loop(stack: List[Tree[T]], output: Queue[S]): Queue[S] = stack match {
    case Nil => output
    case Tree(v,c)::ts => loop(c:::ts,output.enqueue(f(v)))
  }
  loop(tree::Nil, Queue.empty)
}

def postOrderR[T,S](tree: Tree[T], f: T => S): Queue[S] = {
  def loop(stack: List[Tree[T]], output: Queue[S]): Queue[S] = stack match {
    case Nil => output
    case Tree(v,Nil)::ts => loop(ts,output.enqueue(f(v)))
    case Tree(v,c::cs)::ts => loop(c::Tree(v,cs)::ts, output)
  }
  loop(tree::Nil,Queue.empty)
}


def bfs[T,S](tree: Tree[T], f: T => S): Queue[S] = {
  def loop(queue: Queue[Tree[T]], output: Queue[S], visited: Set[Tree[T]]): Queue[S] = queue match {
    case Queue() => output
    case t +:ts if(!visited.contains(t)) => loop(ts.enqueue(t.children), output.enqueue(f(t.value)),visited + t )
    case t +:ts => loop(ts,output,visited)
  }
  loop(Queue(tree),Queue.empty,Set.empty)
}

def bfsAdjList[T,S](list: Map[T,List[T]], root: T, f: T => S): Queue[S] = {
  def loop(queue: Queue[T], output: Queue[S], visited: Set[T]): Queue[S] = queue match {
    case Queue() => output
    case v +: ts if(!visited.contains(v)) => loop(ts.enqueue(list.get(v).getOrElse(Queue.empty)),output.enqueue(f(v)),visited + v)
    case _ +: ts => loop(ts,output,visited)
  }

  loop(Queue(root),Queue.empty,Set.empty)
}



adjList(tree)

val preO = preOrder(tree,identity[Int])
val preOR = preOrderR(tree,identity[Int])
val postO = postOrder(tree,identity[Int])
val postOR = postOrderR(tree,identity[Int])
val inO = inOrder(tree,identity[Int])

val bfsRec = bfs(tree,identity[Int])
val bfsAdj = bfsAdjList(adjList(tree), 1, identity[Int])

