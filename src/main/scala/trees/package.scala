package object trees {

  /**
    *
    *                  1
    *                /   \
    *               2     3
    *             /  \   / \
    *            4    5 6   7
    *
    * Preorder: 1,2,3,4,5,3,6,7
    * InOrder: 4,2,4,1,6,3,7
    * PostOrder: 4,5,2,6,7,3,1
    */



  import scala.annotation.tailrec
  import scala.collection.immutable.Queue

  case class BTree[T](value: T, left: Option[BTree[T]], right: Option[BTree[T]])


  def preOrder[T,S](root: Option[BTree[T]], f: T => S): Queue[S] = {
    def loop(tree: Option[BTree[T]], acc: Queue[S]): Queue[S] =
      tree.fold(acc){case BTree(v,l,r) => loop(r,loop(l,acc.enqueue(f(v))))}
    loop(root,Queue.empty)
  }

  def preOrderC[T,S](root: Option[BTree[T]], f: T => S): Queue[S] = {
    @tailrec
    def loop(stack: List[BTree[T]], acc: Queue[S]): Queue[S] = stack match {
      case Nil => acc
      case BTree(v,l,r)::ts => loop(l.toList:::r.toList:::ts,acc.enqueue(f(v)))
    }
    loop(root.toList,Queue.empty)
  }

  def inOrder[T,S](root: Option[BTree[T]], f: T => S): Queue[S] = {
    def loop(r: Option[BTree[T]], acc: Queue[S]): Queue[S] =
      r.fold(acc){case BTree(v,l,r) => loop(r,loop(l,acc).enqueue(f(v)))}
    loop(root, Queue.empty)
  }

  def inOrderC[T,S](root: Option[BTree[T]], f: T => S): Queue[S] = {
    @tailrec
    def loop(stack: List[BTree[T]], queue: Queue[S]): Queue[S] = stack match {
      case Nil => queue
      case BTree(v,None,r)::ts => loop(r.toList:::ts,queue.enqueue(f(v)))
      case BTree(v,l,r)::ts => loop(l.toList:::BTree(v,None,r)::ts,queue)
    }
    loop(root.toList, Queue.empty)
  }

  def inOrderCB[T,S](root: Option[BTree[T]], f: T => S): Queue[S] = {
    @tailrec
    def loop(stack: List[BTree[T]], queue: Queue[S]): Queue[S] = stack match {
      case Nil => queue
      case BTree(v,None,r)::ts => loop(r.toList:::ts,queue.enqueue(f(v)))
      case BTree(v,l,r)::ts => loop(l.toList:::BTree(v,None,r)::ts,queue)
    }
    loop(root.toList, Queue.empty)
  }

  def postOrder[T,S](root: Option[BTree[T]], f: T => S): Queue[S] = {
    def loop(tree: Option[BTree[T]], queue: Queue[S]): Queue[S] =
      tree.fold(queue) { case BTree(v, l, r) =>
        loop(r,loop(l,queue)).enqueue(f(v))
      }
    loop(root,Queue.empty)
  }


  /**
    *
    * A tail-recursive version
    * @param root: the tree to be traversed, can be empty (None)
    * @param f: The transformation applied to the value of type T
    * @tparam T: The type of the value stored by the tree
    * @tparam S: The transformed type
    * @return A Queue[S]
    *
    * We observe that when doing a post-order traversal, we only evaluate a node
    * only after we evaluated it's left and right children or if it's children are
    * empty. Furthermore, the evaluation of a node only happens after it's children =>
    *
    */
  def postOrderC[T,S](root: Option[BTree[T]], f: T => S): Queue[S] = {
    @tailrec
    def loop(stack: List[BTree[T]], queue: Queue[S]): Queue[S] = stack match {
      case Nil => queue
      case BTree(v,None,None)::ts => loop(ts,queue.enqueue(f(v)))
      case BTree(v,l,r)::ts => loop(l.toList:::r.toList:::BTree(v,None,None)::ts,queue)
    }
    loop(root.toList,Queue.empty)
  }

  import scala.collection.immutable.Queue

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

  val tree = Tree(1,List(
    Tree(2,List(Tree(4,Nil), Tree(5,Nil))),
    Tree(3,List(Tree(6,Nil), Tree(7,Nil))),
    Tree(8,List(Tree(9,Nil), Tree(10,Nil)))
  ))

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



  val preO = preOrder(tree,identity[Int])
  val preOR = preOrderR(tree,identity[Int])
  val postO = postOrder(tree,identity[Int])
  val postOR = postOrderR(tree,identity[Int])
  val inO = inOrder(tree,identity[Int])






  val root = BTree[Int](1,
    Some(BTree(2,
      Some(BTree(4,None,None)),
      //None,
      Some(BTree(5,None,None)))),
    Some(BTree(3,
      Some(BTree(6,None,None)), Some(BTree(7,None,None)))))


  val preR = preOrder(Some(root), identity[Int])
  val preC = preOrderC(Some(root), identity[Int])
  val ioR = inOrder(Some(root), identity[Int])
  val ioC = inOrderC(Some(root), identity[Int])
  val posR = postOrder(Some(root), identity[Int])
  val posC = postOrderC(Some(root),identity[Int])
}
