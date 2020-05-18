
  val size: Int
  val id = (0 until size).toArray zip Array.fill(size)(1)

  def root(i: Int): (Int, Int) = {
    def loop(i: Int, nodes: List[Int]): ((Int, Int), List[Int]) =
      if (id(i)._1 == i) (id(i), nodes)
      else loop(id(id(i)._1)._1, i :: nodes)


    val (r, list) = loop(i, Nil)
    println(s"root for $i is $r")

    println("before")
    id.foreach(pair => print(pair + " "));println()
    list.foreach(id.update(_, r))
    println("after")
    id.foreach(pair => print(pair + ""));println()
    r
  }


  def connected(p: Int, q: Int) = root(p) == root( q)


  def union(p: Int, q: Int) = {
    (root(p), root(q)) match {
      case (rp, rq) if rp == rq => id
      case ((rp, cp), (rq, cq)) => {
        if (cp < cq) {
          id.update(rp, (rq, cp + cq))
          id.update(rq, (rq, cp + cq))
        }
        else {
          id.update(rq, (rp, cp + cq))
          id.update(rp, (rp, cp + cq))
        }
      }
    }
    id

  }








val qf = new QuickFind(4)
val edges = Array(Array(0,1),Array(2,3),Array(1,2))

 edges.foreach(edge => qf.union(edge(0),edge(1)))
(0 until 4).map(i => qf.root(i)).distinct.size
