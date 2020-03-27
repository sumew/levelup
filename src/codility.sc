
def solution(a: Array[Int]): Int = {
  val set = a.toSet

  def loop(count: Int): Int = {
    if (set.contains(count))
      loop(count + 1)
    else count
  }

  loop(1)
}


def checkRow(reserved: Array[String]): Int = {
  if (
    !reserved.contains("B") &&
      !reserved.contains("C") &&
      !reserved.contains("D") &&
      !reserved.contains("E") &&
      !reserved.contains("F") &&
      !reserved.contains("G") &&
      !reserved.contains("H") &&
      !reserved.contains("J")) 2

  else if (
    (
      !reserved.contains("B") &&
        !reserved.contains("C") &&
        !reserved.contains("D") &&
        !reserved.contains("E")) ||
      (
        !reserved.contains("F") &&
          !reserved.contains("G") &&
          !reserved.contains("H") &&
          !reserved.contains("J")) ||
      (
        !reserved.contains("D") &&
          !reserved.contains("E") &&
          !reserved.contains("F") &&
          !reserved.contains("G"))) 1
  else 0


}



"1A 2F 1C".split(" ").map(_.splitAt(1)).groupBy(_._1)
  .map(pair => (pair._1, pair._2.map(_._2)))
def plane(n: Int, s: String): Int = {
  val reserved = s.split(" ").map(_.splitAt(1)).groupBy(_._1)
    .map(pair => (pair._1, pair._2.map(_._2)))

  reserved.foldLeft(0) { (agg, pair) => agg + checkRow(pair._2) }
}


plane(2, "1A 2F 1C")
plane(1,"")





/**
T[0] = "codility1"   R[0] = "Wrong answer"
T[1] = "codility3"   R[1] = "OK"
T[2] = "codility2"   R[2] = "OK"
T[3] = "codility4b"  R[3] = "Runtime error"
T[4] = "codility4a"  R[4] = "OK"
  **/
/**
  *  (['test1a', 'test2', 'test1b', 'test1c', 'test3'],
  *  ['Wrong answer', 'OK', 'Runtime error', 'OK', 'Time limit exceeded'])
  */
val tt = Array("codility1", "codility3", "codility2", "codility4b" ,"codility4a")
val rr = Array("Wrong answer",
  "OK",
  "OK",
  "Runtime error",
  "OK")

def simplify(s: String): String = s.dropWhile( c => !Character.isDigit(c))
def solution(t: Array[String], r: Array[String]): Int = {
  //List(Array(OK), Array(Wrong answer), Array(Runtime error, OK), Array(OK)), ugly but quick
  val resultsPerGroup = t.map(simplify).zip(r).groupBy{ case (k,_ ) => k.charAt(0)}.values.map{ array => array.map(_._2)}
  //Number of  groups passed
  val correctGroups = resultsPerGroup.map(
    _.distinct//Helps equate to "OK"
      .map(_ == "OK")
      .reduce(_ && _))//All need to be true
    .filter(identity).size

  (correctGroups * 100) / resultsPerGroup.size

}

// .groupBy{pair => pair._1.charAt(0)}.map(_._2)
val t = Array("test1a", "test2", "test1b", "test1c", "test3")
val r = Array("Wrong answer", "OK", "Runtime error", "OK", "Time limit exceeded")
val resultsPerGroup = t.map(simplify).zip(r).groupBy{ case (k,v) => k.charAt(0)}.values.map{ array => array.map(_._2)}
resultsPerGroup
solution(t,r)




def removeDuplicates(nums: Array[Int]): Int = {
  var newSize = 0
  if(nums.size <= 2) nums.size
  1 to nums.size - 2 foreach{ idx => {
    if(nums(idx -1) == nums(idx)) {
      newSize +=1
      val fresh = (idx to nums.size-1).filter(nums(_) != nums(idx))
      println(s"index: $idx, fresh: $fresh")
      println(s"nums: ${nums.foreach(println)}")
      val freshIdx = 4
      (idx to freshIdx).foreach(nums(_) = nums(freshIdx))
    }

  }}

  newSize

}

removeDuplicates(Array(1,1,1,1,2,2,2,3,4,5,5,5,6))

/**
  * 1,1,2,3,4,5,5,6
  * 1,2,2,3,4,5,5,6
  * 1,2,3,3,4,5,5,6
  * 1,2,3,4,4,5,5,6
  * 1,2,3,4,5,5,5,6
  *

  */

