package object lists {

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }




  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {


    def appendTo(acc: ListNode, n: ListNode): ListNode = acc match {
      case null => n
      case node => node.next = n; node.next
    }

    def traverse(node: ListNode, rendering: String = ""): String = node match {
      case null => rendering
      case n => traverse(node.next, if(rendering.isEmpty) (n.x).toString else s"$rendering->${n.x}")
    }

    def initial(l: ListNode, r: ListNode): (ListNode,ListNode,ListNode) = (l,r) match {
      case (null, null) => (l,r,l)
      case (l, null) => (l.next,r,l)
      case (null, r) => (l,r.next,r)
      case (l, r) => if(l.x < r.x) (l.next,r,l) else (l,r.next,r)
    }


    val (left,right,init) = initial(l1,l2)
    def loop(one: ListNode, two: ListNode, mergedLastNode: ListNode): ListNode = {
      //println(s"one: ${traverse(one)}, two: ${traverse(two)}, merged: ${traverse(mergedLastNode)}")
      (one, two) match {
        case (null, null) => init
        case (l, null) => appendTo(mergedLastNode, l); init
        case (null, r) => appendTo(mergedLastNode, r); init
        case (l, r) =>
          if (l.x < r.x)
            loop(l.next, r, appendTo(mergedLastNode, l))
          else
            loop(l, r.next, appendTo(mergedLastNode, r))
      }
    }
    loop(left,right,init)

  }


}
