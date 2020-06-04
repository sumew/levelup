def init(size: Int) = (0 until size).toArray.zip(Array.fill(size)(1))


def updateArray(index: Int, pair: (Int,Int), arr: Array[(Int,Int)]) = {
  arr(index) = pair
  arr
}
def root(p: Int, id: Array[(Int,Int)]): (Int,Int) = if(id(p)._1 == p) id(p) else root(id(p)._1, updateArray(p, id(p),id))

def find(p: Int, q: Int, id: Array[(Int,Int)]): Boolean = id(p) == id(q)

def union(p: Int, q: Int, id: Array[(Int,Int)]): Array[(Int,Int)] = {
  (root(p,id), root(q,id)) match {
    case ((rp,sp), (rq,sq)) if sp >= sq => updateArray(rp,(rp,sp+sq),updateArray(rq,(rp,sp+sq),id))
    case ((rp,sp), (rq,sq)) => updateArray(rq,(rq,sp+sq),updateArray(rp,(rq,sp+sq),id))
  }
}

print("wtf")
