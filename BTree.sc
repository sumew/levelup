import trees.{BTree, inOrder, inOrderC, postOrder, postOrderC, preOrder, preOrderC, root}

import scala.collection.immutable.Queue


val preR = preOrder(Some(root), identity[Int])
val preC = preOrderC(Some(root), identity[Int])
val ioR = inOrder(Some(root), identity[Int])
val ioC = inOrderC(Some(root), identity[Int])
val posR = postOrder(Some(root), identity[Int])
val posC = postOrderC(Some(root),identity[Int])


val tt = "scala"
