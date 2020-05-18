/**
  * At each step, enqueue the value at the current node and add
  * the left & right children to the stack. Option's toList
  * helps convert empty children (None values) to an empty list
  */
def preOrderC[T, S](root: Option[BTree[T]], f: T => S): Queue[S] = {
  def loop(stack: List[BTree[T]], acc: Queue[S]): Queue[S] = stack match {
    case Nil => acc
    case BTree(v, l, r) :: ts => loop(l.toList ::: r.toList ::: ts, acc.enqueue(f(v)))
  }
  loop(root.toList, Queue.empty)
}

/**
  * We enqueue a value when we reach a bottom left node (BTree(_,None,_))
  * otherwise, we add the left subtree to the top of the stack while removing
  * it from the current node
  */
def inOrderC[T, S](root: Option[BTree[T]], f: T => S): Queue[S] = {
  def loop(stack: List[BTree[T]], queue: Queue[S]): Queue[S] = stack match {
    case Nil => queue
    case BTree(v, None, r) :: ts => loop(r.toList ::: ts, queue.enqueue(f(v)))
    case BTree(v, l, r) :: ts => loop(l.toList ::: BTree(v, None, r) :: ts, queue)
  }
  loop(root.toList, Queue.empty)
}

/**
  * Enqueue when we encounter a node with no children, otherwise add the left
  * and right child to the stack and remove them from the current node
  */

def postOrderC[T, S](root: Option[BTree[T]], f: T => S): Queue[S] = {
  def loop(stack: List[BTree[T]], queue: Queue[S]): Queue[S] = stack match {
    case Nil => queue
    case BTree(v, None, None) :: ts => loop(ts, queue.enqueue(f(v)))
    case BTree(v, l, r) :: ts => loop(l.toList ::: r.toList ::: BTree(v, None, None) :: ts, queue)
  }
  loop(root.toList, Queue.empty)
}